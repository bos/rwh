{-- snippet roots --}
import Data.Complex

roots :: Double -> Double -> Double
      -> Either (Complex Double, Complex Double) (Double, Double)

roots a b c = if n >= 0
              then Right ((-b + sqrt n) / a2, (-b - sqrt n) / a2)
              else Left ((-b' + sqrt n') / a2', (-b' - sqrt n') / a2')
    where n   = b**2 - 4 * a * c
          a2  = 2 * a
          n'  = n :+ 0
          b'  = b :+ 0
          a2' = a2 :+ 0
{-- /snippet roots --}

{-- snippet isRealValued --}
isRealValued :: Either (Complex Double, Complex Double) (Double, Double)
             -> Bool
isRealValued (Left _) = False
isRealValued _        = True
{-- /snippet isRealValued --}

{-- snippet realRoots --}
realRoots :: Double -> Double -> Double -> Maybe (Double, Double)

realRoots a b c = let n  = b**2 - 4 * a * c
                      a2 = 2 * a
                      r1 = (-b + sqrt n) / a2
                      r2 = (-b - sqrt n) / a2
                  in if n >= 0 && a /= 0
                     then Just (r1, r2)
                     else Nothing
{-- /snippet realRoots --}

{-- snippet hasRealRoots --}
hasRealRoots a b c = case realRoots a b c of
                       Just _  -> True
                       Nothing -> False
{-- /snippet hasRealRoots --}

{-- snippet guardedRoots --}
guardedRoots a b c
    | n >= 0 && a /= 0 = Just (r1, r2)
    | otherwise        = Nothing
    where n  = b**2 - 4 * a * c
          a2 = 2 * a
          r1 = (-b + sqrt n) / a2
          r2 = (-b - sqrt n) / a2
{-- /snippet guardedRoots --}

{-- snippet finalRoots --}
finalRoots 0 _ _             = Nothing
finalRoots a b c | n >= 0    = Just (r1, r2)
                 | otherwise = Nothing
    where n  = b**2 - 4 * a * c
          r1 = (-b + sqrt n) / (2 * a)
          r2 = (-b - sqrt n) / (2 * a)
{-- /snippet finalRoots --}
