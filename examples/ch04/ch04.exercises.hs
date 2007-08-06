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
