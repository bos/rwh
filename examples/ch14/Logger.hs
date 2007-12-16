{-- snippet module --}
module Logger
    (
      Logger
    , Log
    , execLogger
    , record
    ) where
{-- /snippet module --}

import Control.Monad (liftM, liftM2)

{-- snippet Log --}
type Log = [String]
{-- /snippet Log --}

newtype Logger a = Logger { runLogger :: (a, Log) }
    deriving (Show)

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = runLogger m
                  (b, x) = runLogger (k a)
              in Logger (b, w ++ x)

{-- snippet execLogger.type --}
execLogger :: Logger a -> (a, Log)
{-- /snippet execLogger.type --}
execLogger m = runLogger m

{-- snippet record.type --}
record :: String -> Logger ()
{-- /snippet record.type --}
record s = Logger ((), [s])

{-- snippet globToRegex.type --}
globToRegex :: String -> Logger String
{-- /snippet globToRegex.type --}

{-- snippet rooted --}
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)
{-- /snippet rooted --}

{-- snippet eof --}
globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
{-- /snippet eof --}

{-- snippet question --}
globToRegex' ('?':cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)
{-- /snippet question --}

{-- snippet asterisk --}
globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)
{-- /snippet asterisk --}

{-- snippet class --}
globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"
{-- /snippet class --}
{-- snippet last --}
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)
{-- /snippet last --}

{-- snippet escape --}
escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ('\\' : [c])
    | otherwise = return [c]
    where regexChars = "\\+()^$.{}]|"
{-- /snippet escape --}

{-- snippet charClass_wordy --}
charClass_wordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return (']':ds)
charClass_wordy (c:cs) =
    charClass_wordy cs >>= \ds ->
    return (c:ds)
{-- /snippet charClass_wordy --}

{-- snippet charClass --}
charClass :: String -> Logger String
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
charClass [] = fail "unterminated character class"
{-- /snippet charClass --}
