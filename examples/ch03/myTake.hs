{-- snippet myTake --}
myTake :: Int -> [a] -> [a]

myTake n xs = if n <= 0 || null xs
              then xs
              else myTake (n - 1) (tail xs)
{-- /snippet myTake --}

{-- snippet myTake2 --}
myTake2 n xs = if n <= 0 || null xs then xs else myTake (n - 1) (tail xs)
{-- /snippet myTake2 --}
