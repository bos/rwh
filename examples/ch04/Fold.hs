import Prelude hiding (foldl, foldr)

{-- snippet foldl --}
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl f z xs = helper z xs
    where helper z []     = z
          helper z (x:xs) = helper (f z x) xs
{-- /snippet foldl --}

{-- snippet foldr --}
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr f z xs = helper xs
     where helper []     = z
           helper (y:ys) = f y (helper ys)
{-- /snippet foldr --}
