{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

{-- snippet MonadError --}
class (Monad m) => MonadError e m | m -> e where
    throwError :: e             -- error to throw
               -> m a

    catchError :: m a           -- action to execute
               -> (e -> m a)    -- error handler
               -> m a
{-- /snippet MonadError --}

{-- snippet Error --}
class Error a where
    -- create an exception with no message
    noMsg  :: a

    -- create an exception with a message
    strMsg :: String -> a
{-- /snippet Error --}
