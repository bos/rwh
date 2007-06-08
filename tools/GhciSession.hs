{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhciSession
    (
      runScript
    ) where

import Control.Monad (liftM, liftM2, mapM_)
import Control.Monad.State (MonadState(..), StateT(..), gets, modify)
import Control.Monad.Trans (MonadIO(..))
import Data.Char (isSpace)
import Data.List (inits, isPrefixOf, tails)
import Pty (executePseudoTerminal)
import System.Posix.Process (ProcessStatus(..), getProcessStatus)
import System.IO

data SessionState = SessionState
    {
      transcript :: Maybe Handle
    , pty :: Handle
    , output :: String
    }

initialState :: Handle -> String -> SessionState
initialState h s = SessionState
                     {
                       transcript = Nothing
                     , pty = h
                     , output = s
                     }

newtype Session a = Session ((StateT SessionState IO) a)
    deriving (Monad, MonadIO, MonadState SessionState)

runSession :: SessionState -> Session a -> IO ()
runSession st (Session a) = runStateT a st >> return ()

io :: IO a -> Session a
io = liftIO

runScript :: FilePath -> IO (Maybe ProcessStatus)

runScript name = do
    script <- readFile name
    (h, pid) <- executePseudoTerminal "ghci" True [] Nothing
    hs <- hGetContents h
    runSession (initialState h hs) $ do
        setPrompt
        mapM_ processLine (lines script)
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

anyM :: Monad m => (a -> Bool) -> [a] -> m a
anyM _ []                 = fail "no match"
anyM p (x:xs) | p x       = return x
              | otherwise = anyM p xs

findInfix :: Eq a => [a] -> [a] -> Either [a] ([a],[a])
findInfix needle haystack =
    maybe (Left haystack) Right $
          anyM (isPrefixOf needle . snd)
               (liftM2 zip inits tails haystack)

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

debug :: Show a => DebugEvent -> a -> Session ()
debug Write a = io $ putStrLn ("-> " ++ show a)
debug Read a = io $ putStrLn ("<- " ++ show a)
debug Trace a = io $ putStrLn ("-- " ++ show a)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace

changeTranscript :: String -> Session ()
changeTranscript name = do
    oldH <- gets transcript
    maybe (return ()) (io . hClose) oldH
    case name of
      [] -> modify (\st -> st{transcript=Nothing})
      baseName -> do
        let fileName = baseName ++ ".xml"
        newH <- io $ openFile fileName WriteMode
        modify (\st -> st{transcript=Just newH})

processLine :: String -> Session ()
processLine ('-':'-':'#':name) = changeTranscript (strip name)
processLine ('-':'-':_) = return ()
processLine s | all isSpace s = return ()
              | otherwise = do
    putGhci s
    putTranscript ("ghci> " ++ s ++ "\n")
    findOutput (s ++ "\r\n")
    r <- filter (/= '\r') `liftM` findOutput prompt
    debug Read r
    putTranscript r
