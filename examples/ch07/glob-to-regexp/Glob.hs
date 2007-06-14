module Glob (namesMatching) where

import GlobRegex (matchesGlob)

{-- snippet type --}
namesMatching :: String -> IO [FilePath]
{-- /snippet type --}

namesMatching pat = undefined

isMagical :: String -> Bool

isMagical = any (`elem` "[*?")
