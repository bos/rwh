module Arbitrary where

{-- snippet Class --}
class Arbitrary a where
  arbitrary   :: Gen a
{-- snippet /Class --}

--  coarbitrary :: a -> Gen b -> Gen b

{-- snippet IntroductionForms --}
  elements :: [a] -> Gen a
  choose   :: Random a => (a, a) -> Gen a
  oneof    :: [Gen a] -> Gen a
{-- snippet /IntroductionForms --}

{-- snippet Instance --}
instance Arbitrary Bool where
  arbitrary     = elements [True, False]
{-- snippet /Instance --}

--  coarbitrary b = if b then variant 0 else variant 1
