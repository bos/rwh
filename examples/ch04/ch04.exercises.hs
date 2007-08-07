import Data.Char (ord)
import Prelude hiding (concat, takeWhile)

{-- snippet concat --}
concat :: [[a]] -> [a]
{-- /snippet concat --}
concat = foldr (++) []

{-- snippet takeWhile --}
takeWhile :: (a -> Bool) -> [a] -> [a]
{-- /snippet takeWhile --}
takeWhile p = foldr step []
    where step x xs | p x       = x:xs
                    | otherwise = []

{-- snippet groupBy --}
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
{-- /snippet groupBy --}
groupBy f = foldr step []
    where step x [] = [[x]]
          step x ((y:ys):zs) | f x y     = (x:y:ys):zs
                             | otherwise = [x]:(y:ys):zs

-- Idea courtesy of William Lee Irwin.
{-- snippet asInt_fold --}
asInt_fold :: String -> Int
{-- /snippet asInt_fold --}
asInt_fold ('-':xs) = negate (asInt' xs)
asInt_fold xs = asInt_fold' xs

asInt_fold' [] = error "empty string"
asInt_fold' xs = foldr step 0 xs
    where step c n
              | c `elem` ['0'..'9'] = let n' = n * 10 + ord c - zero
                                      in if n' < n
                                         then error "numeric overflow"
                                         else n'
              | otherwise = error ("non-digit " ++ show c)
          zero = ord '0'

{-- snippet asInt_either --}
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
{-- /snippet asInt_either --}
asInt_either ('-':xs) = case asInt_either' xs of
                          Left err -> Left err
                          Right val -> Right (negate val)
asInt_either xs = asInt_either' xs

asInt_either' [] = Left "empty string"
asInt_either' xs = foldr step (Right 0) xs
    where step c (Right n)
              | c `elem` ['0'..'9'] = let n' = n * 10 + ord c - zero
                                      in if n' < n
                                         then Left "numeric overflow"
                                         else Right n'
              | otherwise = Left ("non-digit " ++ show c)
          step _ err = err
          zero = ord '0'
