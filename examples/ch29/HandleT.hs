module HandleT
    (
      MonadHandle(..)
    , HandleT
    , runHandleT
    ) where

import Control.Monad.Trans
import qualified System.IO
import System.IO (Handle, hClose)

class MonadIO m => MonadHandle m where
    hGetContents :: Handle -> m String
    hGetContents = liftIO . System.IO.hGetContents

    hPutStr :: Handle -> String -> m ()
    hPutStr h = liftIO . System.IO.hPutStr h

    hPutStrLn :: Handle -> String -> m ()
    hPutStrLn h = liftIO . System.IO.hPutStrLn h

instance MonadHandle IO

newtype HandleT m a = HandleT { runHandleT :: m a }

instance MonadTrans HandleT where
    lift = HandleT

instance Monad m => Monad (HandleT m) where
    return = HandleT . return
    m >>= k = HandleT $ runHandleT m >>= runHandleT . k
    fail = lift . fail

instance MonadIO m => MonadIO (HandleT m) where
    liftIO = lift . liftIO
