{-- snippet main --}
{-# LANGUAGE PatternSignatures #-}

module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (getStdGen, randoms)

import Sorting

testFunction = parSort2   -- substitute parSort or sort here

main = do
  args <- getArgs
  let count | null args = 100000
            | otherwise = read (head args)
  input :: [Int] <- (take count . randoms) `fmap` getStdGen
  putStrLn $ "we have " ++ show (length input) ++ " elements to sort"
  start <- getCurrentTime
  let sorted = testFunction input
  putStrLn $ "sorted all " ++ show (length sorted) ++ " elements"
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " seconds elapsed"
{-- /snippet main --}
