import Control.Arrow (first)
import Control.Monad (foldM)
import Control.Monad.State (State(..), get, put)
import Data.List (isInfixOf, unfoldr)
import System.Environment (getArgs)
import Text.Regex.Posix ((=~))

foo :: String -> State Int [String]
foo s = case s =~ "(<para)([^>]*)>" of
    (_,"",_,_) -> return []
    (before,_,after,[tag,attrs])
        | " id=\"" `isInfixOf` attrs -> do
          after' <- foo after
          return (before : (tag ++ attrs ++ ">") : after')
        | otherwise -> do
          n <- get
          put (n + 1)
          after' <- foo after
          return (before : (tag ++ attrs ++ " id=\"" ++ toDocBookID n ++
                            "\">") : after')
    _ -> fail "borked regex"

toDocBookID :: Int -> String
toDocBookID n = unfoldr digit (n, firstSyms)
    where digit (_, []) = Nothing
          digit (m, syms) = let (d, r) = m `divMod` length syms
                            in Just (syms !! r,
                                     (d, if d == 0 then [] else restSyms))
          firstSyms = ['A'..'Z'] ++ ['a'..'z']
          restSyms = ['0'..'9'] ++ firstSyms

addTags :: Int -> String -> (String, Int)
addTags num s = first concat (runState (foo s) num)

processFile :: Int -> FilePath -> IO Int
processFile num name = do
  (content, num') <- addTags num `fmap` readFile name
  writeFile (name ++ ".tmp") content
  return num'

main :: IO ()
main = do
  let idPath = ".biggest.id"
  n <- catch (read `fmap` readFile idPath) (const (return 0))
  getArgs >>= foldM processFile n >>= writeFile idPath . show
