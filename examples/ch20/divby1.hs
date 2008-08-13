{-- snippet all --}
-- ch20/divby1.hs

divBy :: Integral a => a -> [a] -> [a]
divBy numerator = map (numerator `div`)
{-- /snippet all --}
