-- This module parses specially delimited sections out a text string,
-- and returns a list of them.  Sections have names.  They can't nest.
-- And start and end pairs must match.
--
-- {-- snippet name_is_foo --}
-- text to be included inside the snippet named "name_is_foo"
-- {-- /snippet name_is_foo --}

module Snippet (
      Snippet(..)
    , parseSnippets
    ) where

import Data.Maybe (isJust)
import Control.Monad (foldM, liftM, when)
import Control.Monad.State (State(..), evalState, get, put)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isAlphaNum, isSpace)

data Snippet = Snippet
    {
      snipName :: B.ByteString
    , snipContent :: B.ByteString
    } deriving (Show)

isInfixOf :: B.ByteString -> B.ByteString -> Bool
isInfixOf needle haystack = any (B.isPrefixOf needle) (B.tails haystack)

maybeAny :: (a -> Bool) -> [a] -> Maybe a
maybeAny p [] = Nothing
maybeAny p (x:xs) | p x = Just x
                  | otherwise = maybeAny p xs

maybeInfixOf :: B.ByteString -> B.ByteString -> Maybe B.ByteString
maybeInfixOf needle haystack =
    maybeAny (B.isPrefixOf needle) (B.tails haystack)

startSnippet = B.pack ('{' : "-- snippet ")
endSnippet = B.pack ('{' : "-- /snippet ")

data CurState = Outside
              | Inside [B.ByteString] B.ByteString 
              deriving (Show)

data S = S
    {
      state :: CurState
    , snippets :: [Snippet]
    } deriving (Show)

emptyS = S Outside []

maybeTag :: B.ByteString -> B.ByteString -> Maybe B.ByteString
maybeTag t line = maybe Nothing (Just . tagName t) (t `maybeInfixOf` line)
    where tagName tag str =
              let name = (B.takeWhile isValidName . B.dropWhile isSpace .
                          B.drop (B.length tag)) str
              in if B.null name
                 then error "empty name"
                 else name
          isValidName c = isAlphaNum c || c `elem` "._"

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing _ = return ()

showB = show . B.unpack

ps :: [B.ByteString] -> State S [Snippet]
ps [] = do
    s <- get
    case state s of
      Outside -> (return . reverse . snippets) s
      Inside _ name -> fail ("unterminated snippet " ++ showB name)
ps (line:lines) = do
    let mStart = maybeTag startSnippet line
        mEnd = maybeTag endSnippet line
    s <- get
    case state s of
      Inside acc name -> do
        whenJust mStart $ \nested ->
            fail ("nested start " ++ showB nested ++ " inside " ++
                  showB name)
        maybe (put s{state = Inside (line:acc) name})
              (\endName ->
                   if name == endName
                   then put s{
                              state = Outside,
                              snippets = Snippet name ((B.concat . reverse) acc) :
                                         snippets s}
                   else fail ("mismatch: start " ++ showB name ++
                              " ends with " ++ showB endName))
              mEnd
      Outside -> do
        whenJust mEnd $ \endName ->
            fail ("end " ++ showB endName ++ " without start")
        whenJust mStart $ \startName ->
            put s{state = Inside [] startName}
    ps lines

parseSnippets s = evalState (ps (B.lines s)) emptyS
