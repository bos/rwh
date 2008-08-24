{-- snippet all --}
divBy :: Integral a => a -> [a] -> Maybe [a]
divBy numerator denominators = 
    mapM (numerator `safeDiv`) denominators
    where safeDiv _ 0 = Nothing
          safeDiv x y = x `div` y
{-- /snippet all --}
