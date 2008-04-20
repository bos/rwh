{-- snippet module --}
import Test.QuickCheck
import Data.List
{-- /snippet module --}

-- Simple model testing

{-- snippet model --}
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] +++ qsort rhs
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs
{-- /snippet model --}

-- Relating functions to other functions

-- Simple function properties

-- Generating random data
