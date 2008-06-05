module Sorting where

{-- snippet parSort --}
import Control.Parallel (par, pseq)

parSort :: (Ord a) => [a] -> [a]

parSort (x:xs)    = force greater `par` (force lesser `pseq`
                                         (lesser ++ x:greater))
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

{-- snippet seqSort --}
seqSort :: (Ord a) => [a] -> [a]

seqSort (x:xs) = greater `pseq` (lesser ++ x:greater)
    where lesser  = seqSort [y | y <- xs, y <  x]
          greater = seqSort [y | y <- xs, y >= x]
seqSort _ = []
{-- /snippet seqSort --}

{-- snippet force --}
force :: [a] -> ()
force (x:xs) = x `pseq` force xs
force _ = ()
{-- /snippet force --}

{-- snippet parSort2 --}
parSort2 :: (Ord a) => Int -> [a] -> [a]
parSort2 d list@(x:xs)
  | d <= 0     = sort list
  | otherwise = force greater `par` (lesser ++ x:greater)
      where lesser      = parSort2 d' [y | y <- xs, y <  x]
            greater     = parSort2 d' [y | y <- xs, y >= x]
            d' = d - 1
parSort2 _ _              = []
{-- /snippet parSort2 --}
