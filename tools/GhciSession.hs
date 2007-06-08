{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhciSession
    (
      runScript
    ) where

import Control.Monad (liftM, mapM_)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.State (MonadState(..), StateT(..), gets, modify)
import Control.Monad.Trans (MonadIO(..))
import Data.Char (isSpace)
import Pty (executePseudoTerminal)
import System.Posix.Process (ProcessStatus(..), getProcessStatus)
import System.IO
import Util (baseName, dropSuffix, findInfix, strip)

data SessionConfig = SessionConfig
    {
      targetName :: FilePath
    }

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

mkConfig :: FilePath -> SessionConfig
mkConfig p = SessionConfig p

newtype Session a = Session (ReaderT SessionConfig (StateT SessionState IO) a)
    deriving (Monad, MonadIO, MonadReader SessionConfig,
              MonadState SessionState)

runSession :: SessionConfig -> SessionState -> Session a -> IO ()
runSession cfg st (Session a) = runStateT (runReaderT a cfg) st >> return ()

io :: IO a -> Session a
io = liftIO

runScript :: FilePath -> FilePath -> IO (Maybe ProcessStatus)

runScript tgtDir name = do
    script <- readFile name
    (h, pid) <- executePseudoTerminal "ghci" True [] Nothing
    hs <- hGetContents h
    let state = mkState h hs
        config = mkConfig (tgtDir ++ '/' : baseName name)
    runSession config state $ do
        setPrompt
        mapM_ processLine (lines script)
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
    findOutput cmd
    return ()

putGhci :: String -> Session ()
putGhci s = do
    debug Write s
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
    out <- gets output
    case findInfix s out of
      Left _ -> fail "could not find a match"
      Right (pfx, prest) -> do
          modify (\st -> st{output=dropLength s prest})
          return pfx

data DebugEvent = Write
                | Read
                | Trace
                  deriving (Eq, Show)

withDebug :: IO () -> Session ()
withDebug f = if False
              then io f
              else return ()

debug :: Show a => DebugEvent -> a -> Session ()
debug Write a = withDebug $ putStrLn ("-> " ++ show a)
debug Read a = withDebug $ putStrLn ("<- " ++ show a)
debug Trace a = withDebug $ putStrLn ("-- " ++ show a)

closeTranscript :: Session ()
closeTranscript = gets transcript >>= maybe (return ()) (\h -> io $ do
    hPutStrLn h "</programlisting>"
    hClose h)

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
    findOutput (s ++ "\r\n")
    r <- filter (/= '\r') `liftM` findOutput prompt
    debug Read r
    putTranscript (q r)
  where ghciPrompt = "<prompt>ghci&gt; </prompt>"
