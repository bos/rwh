module BadPattern where

{-- snippet badExample --}
badExample (x:xs) = x + badExample xs
{-- /snippet badExample --}
