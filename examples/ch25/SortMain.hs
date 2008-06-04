{-- snippet main --}
{-# LANGUAGE PatternSignatures #-}

module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (getStdGen, randoms)

import Sorting

testFunction = parSort2 1   -- substitute parSort or sort here

main = do
  args <- getArgs
  let count | null args = 8192
            | otherwise = read (head args)
  input :: [Int] <- (take count . randoms) `fmap` getStdGen
  putStrLn $ "We have " ++ show (length input) ++ " elements to sort."
  start <- getCurrentTime
  let sorted = testFunction input
  putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
{-- /snippet main --}
