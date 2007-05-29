{-- snippet header --}
module GlobWithRegex
    (
      globToRegex
    , matchesPattern
    ) where
{-- /snippet header --}

{-- snippet imports --}
import System.FilePath (pathSeparator)
import Text.Regex.Posix ((=~))
{-- /snippet imports --}

globToRegex :: String -> String

globToRegex cs = '^' : globToRegex' cs

globToRegex' :: String -> String

globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':cs) = charClass "^[" cs
globToRegex' ('[':cs) = charClass "[" cs
globToRegex' (c:cs)
    | c == escapeChar = let (c', cs') = splitAt 1 cs
                        in '\\' : c' ++ globToRegex' cs'
    | c `elem` "+()^$.{}]|" = '\\' : c : globToRegex' cs
    | otherwise = c : globToRegex' cs
globToRegex' "" = "$"

escapeChar :: Char

escapeChar | pathSeparator == '/' = '\\'
           | pathSeparator == '\\' = '`'
           | otherwise = error "unknown platform in use"

charClass :: String -> String -> String

charClass acc (']':cs) = reverse (']':acc) ++ globToRegex' cs
charClass acc (c:cs) = charClass (c:acc) cs
charClass acc [] = error "unterminated character class"

matchesPattern :: String -> String -> Bool
name `matchesPattern` pat = name =~ (globToRegex pat)
