module Sorting where

{-- snippet parSort --}
import Control.Parallel (par, pseq)

parSort :: (Ord a) => [a] -> [a]

parSort (x:xs)    = force lesser `par` force greater `par`
                    lesser ++ x:greater
    where lesser  = parSort [y | y <- xs, y <  x]
          greater = parSort [y | y <- xs, y >= x]
parSort _         = []
{-- /snippet parSort --}

{-- snippet badSort --}
sillySort (x:xs) = lesser `par` greater `par`
                   lesser ++ x:greater
    where lesser   = sillySort [y | y <- xs, y <  x]
          greater  = sillySort [y | y <- xs, y >= x]
sillySort _        = []
{-- /snippet badSort --}

{-- snippet sort --}
sort :: (Ord a) => [a] -> [a]

sort (x:xs) = lesser ++ x:greater
    where lesser  = sort [y | y <- xs, y <  x]
          greater = sort [y | y <- xs, y >= x]
sort _ = []
{-- /snippet sort --}

{-- snippet force --}
force :: [a] -> ()
force (x:xs) = x `pseq` force xs
force _ = ()
{-- /snippet force --}

{-- snippet parSort2 --}
parSort2 :: (Ord a) => Int -> [a] -> [a]
parSort2 d list@(x:xs)
  | d <= 0     = sort list
  | otherwise = force lesser `par` force greater `par`
                lesser ++ x:greater
      where lesser      = parSort2 d' [y | y <- xs, y <  x]
            greater     = parSort2 d' [y | y <- xs, y >= x]
            d' = d - 1
parSort2 _ _              = []

lengthAtLeast :: Int -> [t] -> Bool
lengthAtLeast k = go 0
    where go n _ | n >= k = True
          go n (x:xs)     = go (n+1) xs
          go n _          = False
{-- /snippet parSort2 --}
