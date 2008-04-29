{-- snippet all --}
-- ch20/divby1.hs

divBy :: Integral a => a -> [a] -> [a]
divBy numerator denominators = 
    map (\x -> numerator `div` x) denominators
{-- /snippet all --}
