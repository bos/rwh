{-# LANGUAGE MultiParamTypeClasses #-}

{-- snippet CustomT --}
newtype CustomT m a = ...
{-- /snippet CustomT --}

{-- snippet mtl --}
instance MonadReader m => MonadReader (CustomT m) where
    ...

instance MonadIO m => MonadIO (CustomT m) where
    ...
{-- /snippet mtl --}
