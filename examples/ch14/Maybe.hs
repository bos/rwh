import Prelude hiding (Maybe(..))

{-- snippet Maybe --}
data Maybe a = Nothing
             | Just a
{-- /snippet Maybe --}
               deriving (Eq, Ord, Show)

{-- snippet chain --}
chain :: m a -> (a -> m b) -> m b
{-- /snippet chain --}
chain = undefined

{-- snippet inject --}
inject :: a -> m a
{-- /snippet inject --}
inject = undefined


{-- snippet Monad --}
class  Monad m  where
    -- chain
    (>>=)  :: m a -> (a -> m b) -> m b
    -- inject
    return :: a -> m a
{-- /snippet Monad --}

{-- snippet fail --}
    fail :: String -> m a
    fail = error
{-- /snippet fail --}

{-- snippet bind_ --}
    (>>) :: m a -> m b -> m b
    a >> f = a >>= \_ -> f
{-- /snippet bind_ --}

{-- snippet fail --}
    fail :: String -> m a
    fail = error
{-- /snippet fail --}
