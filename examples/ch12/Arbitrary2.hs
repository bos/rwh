module Arbitrary2 where

{-- snippet Instance2 --}
instance Arbitrary Bool where
  arbitrary     = do
      n <- choose (0, 1::Int)
      return $ if n == 0 then False else True
{-- snippet /Instance2 --}

