{-# OPTIONS -fglasgow-exts #-}
module Util
    (
      anyM
    , baseName
    , dirName
    , dropSuffix
    , findInfix
    , strip
    ) where

import Control.Monad (liftM2)
import Control.Monad.Instances
import Data.Char (isSpace)
import Data.List (inits, isPrefixOf, tails)

anyM :: Monad m => (a -> Bool) -> [a] -> m a
anyM _ []                 = fail "no match"
anyM p (x:xs) | p x       = return x
              | otherwise = anyM p xs

findInfix :: Eq a => [a] -> [a] -> Either [a] ([a],[a])
findInfix needle haystack =
    maybe (Left haystack) Right $
          anyM (isPrefixOf needle . snd)
               (liftM2 zip inits tails haystack)

baseName :: FilePath -> String
baseName = reverse . takeWhile (/='/') . reverse

dirName :: FilePath -> FilePath
dirName = reverse . either (const ".") (tidy . snd) . findInfix "/" . reverse
    where tidy ('/':cs@(_:_)) = cs
          tidy cs = cs

dropSuffix :: String -> String
dropSuffix = reverse . either id (tail . snd) . findInfix "." . reverse
              
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
