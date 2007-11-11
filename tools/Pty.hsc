{-# LANGUAGE ForeignFunctionInterface #-}

module Pty
    (
      openPseudoTerminal
    , getSlaveTerminalName
    , prepareForLogin
    , forkPseudoTerminal
    , executePseudoTerminal
    , executePseudoTerminalFd
    ) where

#include "Pty.h"

import Control.Monad (when)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt)
import System.IO (Handle, IOMode(..))
import System.Posix.IO ( closeFd, dupTo, stdInput, stdOutput, stdError )
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Terminal (getTerminalName)
import System.Posix.Types
#if __GLASGOW_HASKELL__ >= 608
import System.Posix.Terminal (getSlaveTerminalName, openPseudoTerminal)
import GHC.Handle (fdToHandle')
#else
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString, peekCString)
import System.Posix.IO ( OpenFileFlags(..), OpenMode(..), defaultFileFlags,
                         openFd, )
import qualified GHC.Handle as H (openFd)

openPseudoTerminal :: IO (Fd, Fd)
openPseudoTerminal = do
  (Fd master) <- openFd "/dev/ptmx" ReadWrite Nothing
                        defaultFileFlags{noctty=True}
  throwErrnoIfMinus1_ "openPseudoTerminal" (c_grantpt master)
  throwErrnoIfMinus1_ "openPseudoTerminal" (c_unlockpt master)
  slaveName <- getSlaveTerminalName (Fd master)
  slave <- openFd slaveName ReadWrite Nothing defaultFileFlags{noctty=True}
  return (Fd master, slave)

foreign import ccall unsafe "__pty_grantpt"
  c_grantpt :: CInt -> IO CInt

foreign import ccall unsafe "__pty_unlockpt"
  c_unlockpt :: CInt -> IO CInt

getSlaveTerminalName :: Fd -> IO FilePath
getSlaveTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getSlaveTerminalName" (c_ptsname fd)
  peekCString s

foreign import ccall unsafe "__pty_ptsname"
  c_ptsname :: CInt -> IO CString

fdToHandle' = H.openFd
#endif

prepareForLogin :: Fd -> IO ()
prepareForLogin fd = do
  createSession
  throwErrnoIfMinus1_ "prepareForLogin" (c_setctty fd)
  dupTo fd stdInput
  dupTo fd stdOutput
  dupTo fd stdError
  when (fd > stdError) $
    closeFd fd

foreign import ccall unsafe "__pty_setctty"
  c_setctty :: Fd -> IO CInt

forkPseudoTerminal :: IO () -> IO (Fd, ProcessID)
forkPseudoTerminal child = do
    (master, slave) <- openPseudoTerminal
    pid <- forkProcess $ do
        prepareForLogin slave
        closeFd master
        child
    closeFd slave
    return (master, pid)

executePseudoTerminalFd :: FilePath  -- ^ Command
                        -> Bool   -- ^ Search PATH?
                        -> [String]  -- ^ Arguments
                        -> Maybe [(String, String)]  -- ^ Environment
                        -> IO (Fd, ProcessID)
executePseudoTerminalFd path search args env =
    forkPseudoTerminal (executeFile path search args env)

executePseudoTerminal :: FilePath  -- ^ Command
                        -> Bool   -- ^ Search PATH?
                        -> [String]  -- ^ Arguments
                        -> Maybe [(String, String)]  -- ^ Environment
                        -> IO (Handle, ProcessID)
executePseudoTerminal path search args env = do
    (fd@(Fd fd'), pid) <- forkPseudoTerminal (executeFile path search args env)
    name <- getTerminalName fd                 
    h <- fdToHandle' (fromIntegral fd') Nothing False name ReadWriteMode True
    return (h, pid)
