{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TypeSynonymInstances #-}

module MonadHandle
    (
      MonadHandle(..)
    , HandleT
    , runHandleT
    ) where

import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Writer

{-- snippet MonadHandle --}
import qualified System.IO
import System.IO (IOMode(..))
import System.Directory (removeFile)

class Monad m => MonadHandle h m | m -> h where
    openFile :: FilePath -> IOMode -> m h
    hPutStrLn :: h -> String -> m ()
    hClose :: h -> m ()
{-- /snippet MonadHandle --}

{-- snippet IO --}
instance MonadHandle System.IO.Handle IO where
    openFile = System.IO.openFile
    hPutStrLn = System.IO.hPutStrLn
    hClose = System.IO.hClose
{-- /snippet IO --}

{-- snippet safeHello --}
safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h
{-- /snippet safeHello --}

{-- snippet tidyHello --}
tidyHello :: (MonadIO m, MonadHandle h m) => FilePath -> m ()
tidyHello path = do
  safeHello path
  liftIO (removeFile path)
{-- /snippet tidyHello --}

{-- snippet tidierHello --}
class (MonadHandle h m, MonadIO m) => MonadHandleIO h m | m -> h

instance MonadHandleIO System.IO.Handle IO

tidierHello :: (MonadHandleIO h m) => FilePath -> m ()
tidierHello path = do
  safeHello path
  liftIO (removeFile path)
{-- /snippet tidierHello --}

{-- snippet Event --}
data Event = Open FilePath IOMode
           | PutStrLn String String
           | Close String
             deriving (Show)
{-- /snippet Event --}

{-- snippet WriterIO --}
newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Monad, MonadWriter [Event])
{-- /snippet WriterIO --}

{-- snippet runWriterIO --}
runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW
{-- /snippet runWriterIO --}

instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStrLn h str = tell [PutStrLn h str]
    hClose h = tell [Close h]

newtype HandleT m a = HandleT { runHandleT :: m a }

instance MonadTrans HandleT where
    lift = HandleT

instance Monad m => Monad (HandleT m) where
    return = HandleT . return
    m >>= k = HandleT $ runHandleT m >>= runHandleT . k
    fail = lift . fail

instance MonadIO m => MonadIO (HandleT m) where
    liftIO = lift . liftIO
