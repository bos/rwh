{-- snippet dlts --}
import Data.List (isPrefixOf)

dlts :: String -> [String]

dlts = foldr step [] . lines
{-- /snippet dlts --}
{-- snippet step --}
  where step l ds
          | "#define DLT_" `isPrefixOf` l = (head . drop 1 . words) l : ds
          | otherwise = ds
{-- /snippet step --}

