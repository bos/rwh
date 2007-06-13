{-# OPTIONS_GHC -fno-implicit-prelude #-}

{-- snippet append --}
(++)         :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
{-- /snippet append --}
