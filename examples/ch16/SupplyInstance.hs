{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
    GeneralizedNewtypeDeriving #-}

import SupplyClass
import RandomSupply

{-- snippet Reader --}
newtype Reader i a = R { runReader :: i -> a }
{-- /snippet Reader --}

{-- snippet Monad --}
instance Monad (Reader i) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r
{-- /snippet Monad --}

{-- snippet ask --}
ask :: Reader i i
ask = R id
{-- /snippet ask --}

instance MonadSupply s (Reader s) where
    next = R return

{-- snippet MySupply --}
newtype MySupply i a = MySupply { runMySupply :: Reader i a }
    deriving (Monad, MonadSupply i)
{-- /snippet MySupply --}

{-- snippet runMS --}
runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply
{-- /snippet runMS --}

{-- snippet xy --}
xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)
{-- /snippet xy --}
