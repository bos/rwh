import Data.Complex

{-- snippet realRoots --}
realRoots :: Double -> Double -> Double -> Maybe (Double, Double)

realRoots a b c = let n  = b**2 - 4 * a * c
                      a2 = 2 * a
                      r1 = (-b + sqrt n) / a2
                      r2 = (-b - sqrt n) / a2
                  in if n > 0 && a /= 0
                     then Just (r1, r2)
                     else Nothing
{-- /snippet realRoots --}

roots :: Double -> Double -> Double
      -> Either (Complex Double, Complex Double) (Double, Double)

roots a b c = let n  = b**2 - 4 * a * c
                  a2 = 2 * a
                  n' = n :+ 0
                  b' = b :+ 0
                  a2' = a2 :+ 0
              in if n > 0
                 then Right ((-b + sqrt n) / a2, (-b - sqrt n) / a2)
                 else Left ((-b' + sqrt n') / a2', (-b' - sqrt n') / a2')
