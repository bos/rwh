{-- snippet all --}
import Data.List
import System.IO
import Control.Monad(when)
import System.Exit
import System.Environment(getArgs)
{-
main = do
    -- Load the command-line arguments
    args <- getArgs

    -- If we don't have the right amount of args, give an error and abort
    when (length args /= 2) $ do
        putStrLn "Syntax: passwd-al filename uid"
        exitFailure

    -- Read the file lazily
    content <- readFile (args !! 0)

    -- Compute the username in pure code
    let username = findByUID content (read (args !! 1))

    -- Display the result
    case username of 
         Just x -> putStrLn username
         Nothing -> putStrLn "Could not find that UID"

findByUID :: String -> Integer -> Maybe String
findByUID content uid =
    let al = map parseline . lines $ content
        in lookup uid al

-- parseline :: String -> [(Integer, String)]
-- parseline input =

-}
split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim str =
    let (before, remainder) = span (/= delim) str
        in
        before : case remainder of
                      [] -> []
                      x -> split delim (tail x)

{-- /snippet all --}
