data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Red _ = False
colorEq Green Green = True
colorEq Green _ = False
colorEq Blue Blue = True
colorEq Blue _ = False

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

{-- snippet basiceq --}
class BasicEq a where
    isEqual :: a -> a -> Bool
{-- /snippet basiceq --}

{-- snippet basiceq2 --}
class BasicEq2 a where
    isEqual2 :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool
{-- /snippet basiceq2 --}

{-- snippet basiceq3 --}
class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)
{-- /snippet basiceq3 --}

{-- snippet basiceq3inst --}
instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Red _ = False
    isEqual3 Green Green = True
    isEqual3 Green _ = False
    isEqual3 Blue Blue = True
    isEqual3 Blue _ = False
{-- /snippet basiceq3inst --}

{-- snippet show --}
instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"
{-- /snippet show --}
