{-- snippet rootMeanSquare --}
rootMeanSquare :: [Double] -> Double

rootMeanSquare xs = sqrt (sum (map square xs) / fromIntegral (length xs))
    where square x = x ** 2
{-- /snippet rootMeanSquare --}

rootMeanSquare_explicit xs = sqrt (meanSquare xs 0 0)
    where meanSquare [] i ms     = ms / fromIntegral i
          meanSquare (x:xs) i ms = meanSquare xs (i+1) (ms + x**2)

{-- snippet rootMeanSquare_foldl --}
rootMeanSquare_foldl xs = let (length, meanSquare) = foldl step (0,0) xs
                     in sqrt (meanSquare / fromIntegral length)
    where step (length, meanSquare) x = (length + 1, meanSquare + x**2)
{-- /snippet rootMeanSquare_foldl --}
