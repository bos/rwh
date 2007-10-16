{-# OPTIONS_GHC -fglasgow-exts #-}
import System.Process
import Control.Concurrent
import System.IO
import System.Exit

{- | Storage for the Handles that we will be passing about -}
data Handles = Handles {
    inp :: Handle,
    out :: Handle,
    err :: Handle,
    getExitCode :: IO ExitCode}

type Command = (String, [String])

class Pipeable a b c where
    (-|-) :: a -> b -> IO c

instance Pipeable Handles Handles Handles where
    (-|-) src dest =
        do forkIO (copy (out src) (inp dest))
           forkIO (copy (err src) stdout)
           return $ Handles {inp = (inp src),
                             out = (out dest),
                             err = (err dest),
                             getExitCode = getEC src dest}
 
getEC :: Handles -> Handles -> IO ExitCode
getEC src dest =
    do sec <- getExitCode src
       dec <- getExitCode dest
       case sec of
            ExitSuccess -> return dec
            x -> return x

copy :: Handle -> Handle -> IO ()
copy src dest =
    do c <- hGetContents src
       hPutStr dest c

