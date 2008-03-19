import Prelude hiding (odd)

{-- snippet odd --}
odd n = n `mod` 2 == 1
{-- /snippet odd --}

{-- snippet demo --}
roundToEven n = if odd n
                then if n < 0
                     then n + 1
                     else n - 1
                else n

demo k = let n = k + 5
             r = roundToEven n
         in putStrLn ("n: " ++ show n ++ ", r: " ++ show r)
{-- /snippet demo --}
