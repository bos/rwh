import Prelude hiding ((++))

{-- snippet concat --}
(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x : xs ++ ys
_ ++ ys = ys
{-- /snippet concat --}
