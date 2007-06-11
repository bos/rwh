{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhciSession
    (
      SessionOptions(..)
    , mkOptions
    , runScript
    ) where

import Control.Exception (bracket)
import Control.Monad (liftM, mapM_, when)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.State (MonadState(..), StateT(..), gets, modify)
import Control.Monad.Trans (MonadIO(..))
import Data.Char (isSpace)
import Pty (executePseudoTerminal)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Posix.Process (ProcessStatus(..), getProcessStatus)
import System.IO
import Util (baseName, dirName, dropSuffix, findInfix, strip)

data SessionOptions = SessionOptions
    {
      optHelp :: Bool
    , optVerbose :: Bool
    }


mkOptions :: SessionOptions
mkOptions = SessionOptions
            {
              optHelp = False
            , optVerbose = False
            }

data SessionConfig = SessionConfig
    {
      options :: SessionOptions
    , targetName :: FilePath
    }

mkConfig :: SessionOptions -> FilePath -> SessionConfig
mkConfig o p = SessionConfig o p

data SessionState = SessionState
    {
      transcript :: Maybe Handle
    , pty :: Handle
    , output :: String
    }

mkState :: Handle -> String -> SessionState
mkState h s = SessionState
              {
                transcript = Nothing
              , pty = h
              , output = s
              }

newtype Session a = Session (ReaderT SessionConfig (StateT SessionState IO) a)
    deriving (Monad, MonadIO, MonadReader SessionConfig,
              MonadState SessionState)

runSession :: SessionConfig -> SessionState -> Session a -> IO ()
runSession cfg st (Session a) = runStateT (runReaderT a cfg) st >> return ()

io :: IO a -> Session a
io = liftIO

runScript :: SessionOptions -> FilePath -> FilePath -> IO (Maybe ProcessStatus)

runScript opts tgtDir name = do
    script <- readFile name
    bracket getCurrentDirectory
            (\prevCwd -> do
               cwd <- getCurrentDirectory
               when (cwd /= prevCwd) (setCurrentDirectory prevCwd))
            (\_ -> do
               when ('/' `elem` name) $ do
                   setCurrentDirectory (dirName name)
               runScript' opts tgtDir (baseName name) (lines script))

runScript' :: SessionOptions -> FilePath -> String -> [String]
          -> IO (Maybe ProcessStatus)
runScript' opts tgtDir name script = do
    (h, pid) <- executePseudoTerminal "ghci" True [] Nothing
    hs <- hGetContents h
    let state = mkState h hs
        config = mkConfig opts (tgtDir ++ '/' : name)
    runSession config state $ do
        setPrompt
        mapM_ processLine script
        closeTranscript
    hClose h
    getProcessStatus True False pid

prompt :: String
prompt = "__ghci_prompt__ "

setPrompt :: Session ()
setPrompt = do
    let cmd = ":set prompt \"" ++ prompt ++ "\""
    putGhci cmd
    findOutput cmd
    findOutput (cmd ++ "\r\n")
    return ()

putGhci :: String -> Session ()
putGhci s = do
    debug (Write s)
    h <- gets pty
    io $ hPutStr h (s ++ "\n")

putTranscript :: String -> Session ()
putTranscript s = do
    mh <- gets transcript
    case mh of
      Nothing -> return ()
      Just h -> io $ hPutStr h s

dropLength :: [a] -> [b] -> [b]
dropLength [] ys = ys
dropLength _ [] = []
dropLength (_:xs) (_:ys) = dropLength xs ys

findOutput :: String -> Session String
findOutput s = do
    debug (Trace $ "waiting for " ++ show s)
    out <- gets output
    case findInfix s out of
      Left _ -> fail "could not find a match"
      Right (pfx, prest) -> do
          modify (\st -> st{output=dropLength s prest})
          return pfx

data DebugEvent = Write String
                | Read String
                | Trace String
                  deriving (Eq, Show)

withDebug :: IO () -> Session ()
withDebug f = do
    verbose <- askOpt optVerbose
    when verbose $
        io f

debug :: DebugEvent -> Session ()
debug (Write a) = withDebug $ hPutStrLn stderr ("-> " ++ show a)
debug (Read a) = withDebug $ hPutStrLn stderr ("<- " ++ show a)
debug (Trace a) = withDebug $ hPutStrLn stderr ("-- " ++ a)

closeTranscript :: Session ()
closeTranscript = gets transcript >>= maybe (return ()) (\h -> do
  debug $ Trace ("closing " ++ show h)
  io $ do
    hPutStrLn h "</programlisting>"
    hClose h)

askOpt :: (SessionOptions -> a) -> Session a
askOpt f = f `liftM` asks options

changeTranscript :: String -> Session ()
changeTranscript name = do
    closeTranscript
    case name of
      [] -> modify (\st -> st{transcript=Nothing})
      idName -> do
        tgtName <- asks targetName
        let filePath = tgtName ++ ':' : baseName idName ++ ".xml"
            fileBase = baseName filePath
            tag = dropSuffix fileBase
        debug $ Trace ("opening " ++ filePath)
        newH <- io $ openFile filePath WriteMode
        io $ hPutStrLn newH ("<programlisting id=" ++ show tag ++ ">\n")
        io $ putStrLn ("<!ENTITY " ++ tag ++
                       " SYSTEM " ++ show fileBase ++ ">")
        modify (\st -> st{transcript=Just newH})

q :: String -> String
q ('<':cs) = "&lt;" ++ q cs
q ('>':cs) = "&gt;" ++ q cs
q ('&':cs) = "&amp;" ++ q cs
q (c:cs) = c : q cs
q [] = []

u :: String -> String
u cs = "<userinput>" ++ q cs ++ "</userinput>"

-- FIXME: expand tabs.

processLine :: String -> Session ()
processLine ('-':'-':'#':name) = changeTranscript (strip name)
processLine ('-':'-':_) = return ()
processLine s | all isSpace s = return ()
              | otherwise = do
    putGhci s
    putTranscript (ghciPrompt ++ u s ++ "\n")
    echo <- findOutput "\r\n"
    debug (Trace $ "ghci echoed " ++ show echo)
    r <- filter (/= '\r') `liftM` findOutput prompt
    debug (Read r)
    putTranscript (q r)
  where ghciPrompt = "<prompt>ghci&gt; </prompt>"
