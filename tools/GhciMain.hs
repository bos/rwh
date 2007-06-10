module Main where

import Control.Monad (mapM_, when)
import Data.List (foldl')
import GhciSession (SessionOptions(..), mkOptions, runScript)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..),
                              getOpt', usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (Handle, hPutStr, hPutStrLn, stderr)

options :: [OptDescr (SessionOptions -> SessionOptions)]
options =
    [ Option "h?" ["help"] (NoArg (\s->s{optHelp=True}))
                 "print verbose output"
    , Option "v" ["verbose"] (NoArg (\s->s{optVerbose=True}))
                 "print verbose output"
    ]

usage :: Handle -> Int -> IO ()
usage h n = do
    hPutStr h (usageInfo "Usage: ghcisession [opts] [file ...]" options)
    exitWith $ if n == 0 then ExitSuccess else ExitFailure n

getOptions :: IO (SessionOptions, [String])
getOptions = do
    sysArgs <- getArgs
    let (optfns, args, unknown, errs) = getOpt' RequireOrder options sysArgs
    when ((not . null) unknown) $ do
        mapM_ (hPutStrLn stderr . ("Unknown option: " ++)) unknown
    when ((not . null) errs) $ do
        mapM_ (hPutStrLn stderr) errs
        usage stderr 1
    let opts = foldl' (flip id) mkOptions optfns
    when (optHelp opts) $
        usage stderr 0
    return (opts, args)

main :: IO ()
main = do
    (opts, args) <- getOptions
    when (length args < 2) $
        usage stderr 1
    let (tgtDir:scripts) = args
    mapM_ (runScript opts tgtDir) scripts
