import Prelude hiding (filter, foldl, foldr)

{-- snippet foldl --}
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl f z xs = step z xs
    where step z []     = z
          step z (x:xs) = step (f z x) xs
{-- /snippet foldl --}

{-- snippet foldr --}
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr f z xs = step xs
     where step []     = z
           step (y:ys) = f y (step ys)
{-- /snippet foldr --}

{-- snippet myMap --}
myMap :: (a -> b) -> [a] -> [b]

myMap f xs = foldr step [] xs
    where step x [] = [f x]
          step x ys = f x : ys
{-- /snippet myMap --}

{-- snippet myFoldl --}
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)
{-- /snippet myFoldl --}

{-- snippet filter --}
filter :: (a -> Bool) -> [a] -> [a]
filter p []   = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
{-- /snippet filter --}

{-- snippet myFilter --}
myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys
{-- /snippet myFilter --}

{-- snippet identity --}
identity :: [a] -> [a]
identity xs = foldr (:) [] xs
{-- /snippet identity --}

{-- snippet append --}
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
{-- /snippet append --}
              
