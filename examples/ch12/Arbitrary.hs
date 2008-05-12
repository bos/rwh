module Arbitrary where

{-- snippet Class --}
class Arbitrary a where
  arbitrary   :: Gen a
  coarbitrary :: a -> Gen b -> Gen b
{-- snippet /Class --}

{-- snippet Instance --}
instance Arbitrary Bool where
  arbitrary     = elements [True, False]
  coarbitrary b = if b then variant 0 else variant 1
{-- snippet /Instance --}
