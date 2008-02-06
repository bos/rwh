-- | Take a JSON-format Django "fixture" file, as output by Django's
-- dumpdata command, and turn it into two Haskell data files.

module Main where

import Control.Monad
import Data.List
import Comment
import JSONParsec
import JSON
import System.Environment
import System.IO

readComments :: FilePath -> IO ([Comment], [Element])
readComments path = do
  p <- parseFromFile p_text path
  case p of
    Left err -> fail (show err)
    Right ma -> case fromJValue ma of
                 Just a -> return $ foldl' slurp ([], []) (fromJArray a)
                 Nothing -> fail "barf"
  where slurp (cs, es) v =
            let a = fromJObject $ v
            in (update "comments.comment" toComment cs a,
                update "comments.element" toElement es a)

update :: JSON k => String -> (k -> [(String, JValue)] -> a) -> [a]
       -> [(String, JValue)] -> [a]
update k mkx xs vs
    | any (==("model", JString k)) vs =
        let pk = get "pk" vs
        in look xs ((:xs) . mkx pk . fromJObject) "fields" vs
    | otherwise = xs

look :: JSON a => b -> (a -> b) -> String -> [(String, JValue)] -> b
look bail f k vs = maybe bail f (lookup k vs >>= fromJValue)

toComment :: Int -> [(String, JValue)] -> Comment
toComment _ vs = Comment (get "element" vs)
                         (get "comment" vs)
                         (get "submitter_name" vs)
                         (get "submitter_url" vs)
                         (get "ip" vs)
                         (get "date" vs)
                         (toEnum $ get "reviewed" vs)
                         (toEnum $ get "hidden" vs)

get :: JSON a => String -> [(String, JValue)] -> a
get k vs = look (error $ "missing " ++ show k) id k vs

toElement :: ElementID -> [(String, JValue)] -> Element
toElement pk vs = Element pk
                          (get "chapter" vs)
                          (get "title" vs)

main :: IO ()
main = do
  args <- getArgs
  ch <- openFile "comments.dat" WriteMode
  eh <- openFile "elements.dat" WriteMode
  forM_ args $ \arg -> do
    (cs, es) <- readComments arg
    forM_ cs (hPrint ch)
    forM_ es (hPrint eh)
