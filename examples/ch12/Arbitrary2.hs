module Arbitrary2 where

import Prelude hiding (Bool(..))
import Test.QuickCheck

-- define our own
data Bool = True | False

{-- snippet Instance2 --}
instance Arbitrary Bool where
  arbitrary     = do
      n <- choose (0, 1) :: Gen Int
      return $ if n == 0 then False else True
{-- /snippet Instance2 --}

