{-- snippet color --}
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Red _ = False
colorEq Green Green = True
colorEq Green _ = False
colorEq Blue Blue = True
colorEq Blue _ = False
{-- /snippet color --}

{-- snippet string --}
stringEq :: [Char] -> [Char] -> Bool
-- Match if both are empty
stringEq [] [] = True
-- Evaluate when we have only one character in both
stringEq [x] [y] = x == y
-- If both start with the same char, check the rest
stringEq (x:xs) (y:ys) = 
    if x == y
        then stringEq xs ys
        else False
-- Everything else doesn't match
stringEq _ _ = False
{-- /snippet string --}
