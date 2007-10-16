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

class CommandLike a where
    invoke :: a -> IO Handles

instance CommandLike Command where
    invoke (cmd, args) =
        do (newinp, newout, newerr, ph) <-
                runInteractiveProcess cmd args Nothing Nothing
           return $ Handles newinp newout newerr (waitForProcess ph)

instance CommandLike Handles where
    invoke h = return h

class Pipeable a b c where
    (-|-) :: a -> b -> IO c

instance (CommandLike a, CommandLike b) => 
         Pipeable a b Handles where
    (-|-) srcCmd destCmd =
        do src <- invoke srcCmd
           dest <- invoke destCmd
           forkIO (copy (out src) (inp dest))
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

