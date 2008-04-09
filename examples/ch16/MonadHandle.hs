{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TypeSynonymInstances #-}

module MonadHandle
    (
      MonadHandle(..)
    , HandleT
    , runHandleT
    ) where

import Control.Monad.Trans
import Control.Monad.Writer
import qualified System.IO
import System.IO (IOMode(..))

class Monad m => MonadHandle h m | m -> h where
    openFile :: FilePath -> IOMode -> m h
    hPutStrLn :: h -> String -> m ()
    hClose :: h -> m ()

instance MonadHandle System.IO.Handle IO where
    openFile = System.IO.openFile
    hPutStrLn = System.IO.hPutStrLn
    hClose = System.IO.hClose

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h

data Event = Open FilePath IOMode
           | PutStrLn String String
           | Close String
             deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

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
