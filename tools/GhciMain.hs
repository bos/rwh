module Main where

import Control.Monad (mapM_, when)
import GhciSession (runScript)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()

main = do
    args <- getArgs
    when (length args < 2) $ do
        hPutStrLn stderr "Usage: ghcisession target-dir script [...]"
        exitFailure
    let (tgtDir:scripts) = args
    mapM_ (runScript tgtDir) scripts
