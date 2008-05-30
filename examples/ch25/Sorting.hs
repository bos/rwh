module Sorting where

{-- snippet parSort --}
import Control.Parallel (par)

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
force (x:xs) = x `seq` force xs
force _ = ()
{-- /snippet force --}

{-- snippet parSort2 --}
parSort2 :: (Ord a) => [a] -> [a]
parSort2 list@(x:xs)
  | lengthAtLeast 32 xs = force lesser `par` force greater `par`
                          lesser ++ x:greater
  | otherwise           = sort list
      where lesser      = parSort2 [y | y <- xs, y <  x]
            greater     = parSort2 [y | y <- xs, y >= x]
parSort2 _              = []

lengthAtLeast :: Int -> [t] -> Bool
lengthAtLeast k = go 0
    where go n _ | n >= k = True
          go n (x:xs)     = go (n+1) xs
          go n _          = False
{-- /snippet parSort2 --}
