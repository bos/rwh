{-- snippet all --}
divBy :: Integral a => a -> [a] -> [Maybe a]
divBy numerator denominators =
    map worker denominators
    where worker 0 = Nothing
          worker x = Just (numerator `div` x)
{-- /snippet all --}
