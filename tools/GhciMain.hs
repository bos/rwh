module Main where

import Control.Monad (mapM_)
import GhciSession (runScript)
import System.Environment (getArgs)

main :: IO ()

main = do
    args <- getArgs
    mapM_ runScript args
