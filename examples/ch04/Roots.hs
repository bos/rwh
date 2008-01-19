{-- snippet roots --}
import Data.Complex

data QuadraticRoots = Undefined
                    | RealValued Double Double
                    | ComplexValued (Complex Double) (Complex Double)

roots :: Double -> Double -> Double -> QuadraticRoots

roots a b c =
    if a == 0
    then Undefined
    else if n >= 0
         then RealValued ((-b + s) / a2) ((-b - s) / a2)
         else ComplexValued ((-b' + s') / a2') ((-b' - s') / a2')
  where n   = b**2 - 4 * a * c
        a2  = 2 * a
        n'  = n :+ 0
        b'  = b :+ 0
        a2' = a2 :+ 0
        s = sqrt n
        s' = sqrt n'
{-- /snippet roots --}

{-- snippet isRealValued --}
isRealValued :: Maybe (Double, Double) -> Bool
isRealValued (Just _) = True
isRealValued _        = False
{-- /snippet isRealValued --}

{-- snippet realRoots --}
-- (b^2 - 4ac) / 2a
realRoots :: Double -> Double -> Double -> Maybe (Double, Double)

realRoots a b c = let n  = b^2 - 4 * a * c
                      a2 = 2 * a
                      s = sqrt n
                  in if a /= 0 && n >= 0
                     then Just ((-b + s) / a2, (-b - s) / a2)
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
finalRoots :: Double -> Double -> Double -> QuadraticRoots

finalRoots 0 b c = Undefined
finalRoots a b c
    | n >= 0     = RealValued ((-b + s) / a2) ((-b - s) / a2)
    | otherwise  = ComplexValued ((-b' + s') / a2') ((-b' - s') / a2')
  where n   = b**2 - 4 * a * c
        a2  = 2 * a
        n'  = n :+ 0
        b'  = b :+ 0
        a2' = a2 :+ 0
        s = sqrt n
        s' = sqrt n'
{-- /snippet finalRoots --}
