module TypeConstraint where

{-- snippet OrdStack --}
data (Ord a) => OrdStack a = Bottom
                           | Item a (OrdStack a)
                             deriving (Show)
{-- /snippet OrdStack --}

{-- snippet isMonotonic --}
isMonotonic :: (Ord a) => OrdStack a -> Bool
isMonotonic (Item a rest@(Item b _))
    | compare a b `elem` [GT, LT] = isMonotonic rest
    | otherwise = False
isMonotonic _ = True
{-- /snippet isMonotonic --}

{-- snippet push --}
push :: (Ord a) => a -> OrdStack a -> OrdStack a
push a s = Item a s
{-- /snippet push --}
