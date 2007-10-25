{-# OPTIONS_GHC -fglasgow-exts #-}

module RunProcess where

import System.Process
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import Text.Regex

{- | The type for running external commands.  The first part
of the tuple is the program name.  The list represents the
command-line parameters to pass to the command. -}
type SysCommand = (String, [String])

{- | The type for running Haskell functions and IO actions. -}
type HsCommand = String -> IO String

{- | The result of running any command -}
data CommandResult = CommandResult {
    cmdOutput :: String,              -- ^ IO action that yields the output
    getExitStatus :: IO ProcessStatus    -- ^ IO action that yields exit result
    }

{- | The type for handling global lists of FDs to always close in the clients
-}
type CloseFDs = MVar [Fd]

{- | Class representing anything that is a runnable command -}
class CommandLike a where
    {- | Given the command and a String representing input,
         invokes the command.  Returns a String
         representing the output of the command. -}
    invoke :: a -> ClientFDs -> String -> IO String

-- Support for running system commands
instance CommandLike SysCommand where
    invoke (cmd, args) closefds input =
        do putStrLn $ "35: " ++ cmd ++ " " ++ show args
           (stdinread, stdinwrite) <- createPipe
           (stdoutread, stdoutwrite) <- createPipe

           -- We add the parent FDs to this list because we always need
           -- to close them in the clients.
           addCloseFDs closefds [stdinwrite, stdoutread]
           childPID <- withMVar clientfds (\closefds ->
                          forkProcess (child closefds stdinread stdoutwrite))

           -- Now, on the parent, close the client-side FDs.
           closeFd stdinread
           closeFd stdoutwrite

           -- Write the input to the command.
           stdinhdl <- fdToHandle stdinwrite
           forkIO $ do hPutStr stdinhdl input
                       hClose stdinhdl

           -- Prepare to receive output from the command
           stdouthdl <- fdToHandle stdoutread
           output <- hGetContents stdouthdl
           let waitfunc = 
               do status <- getProcessStatus True False childPID
                  case status of
                       Nothing -> fail $ "Error: Nothing from getProcessStatus"
                       Just ps -> do removeCloseFDs [stdinwrite, stdoutread]
                                     return ps
           return $ CommandResult {cmdOutput = output,
                                   getExitStatus = waitfunc}

        where child closefds stdinread stdoutwrite = 
                do dupTo stdinread stdInput
                   dupTo stdoutwrite stdOutput
                   closeFd stdinread
                   closeFd stdoutwrite
                   mapM_ closeFd closefds
                   executeFile cmd True args Nothing

-- Support for running Haskell commands
instance CommandLike HsCommand where
    invoke func _ input =
       return $ CommandResult (func input) (return (Exited ExitSuccess))

{- | Type representing a pipe.  A 'PipeCommand' consists of a source
and destination part, both of which must be instances of
'CommandLine'. -}
data (CommandLike src, CommandLike dest) => 
     PipeCommand src dest = PipeCommand src dest 

{- | A convenient function for creating a 'PipeCommand'. -}
(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

{- | Make 'PipeCommand' runnable as a command -}
instance (CommandLike a, CommandLike b) =>
         CommandLike (PipeCommand a b) where
    invoke (PipeCommand src dest) closefds input =
        do res1 <- invoke src closefds input
           putStrLn "62"
           putStrLn "64"
           res2 <- invoke dest closefds (cmdOutput res1)
           putStrLn "66"
           return $ CommandResult (cmdOutput res2) (getEC res1 res2)

{- | Utility function to copy data from one Handle to another. -}
copy :: Handle -> Handle -> IO ()
copy src dest =
    do c <- hGetContents src
       hPutStr dest c

{- | Given two 'CommandResult' items, evaluate the exit codes for
both and then return a "combined" exit code.  This will be ExitSuccess
if both exited successfully.  Otherwise, it will reflect the first
error encountered. -}
getEC :: CommandResult -> CommandResult -> IO ExitCode
getEC src dest =
    do sec <- getExitStatus src
       dec <- getExitStatus dest
       case sec of
            Exited ExitSuccess -> return dec
            x -> return x

{- | Execute a 'CommandLike'. -}
runIO :: CommandLike a => a -> IO ()
runIO cmd =
    do res <- invoke cmd (Left [])
       putStrLn "91"
       putStr res
       putStrLn "95"
       ec <- getExitStatus res
       putStrLn "97"
       case ec of
            Exited ExitSuccess -> return ()
            x -> fail $ "Exited: " ++ show x

-- | Count the lines in the input
countLines :: String -> IO String
countLines = return . (++) "\n" . show . length . lines

grep :: String -> String -> IO String
grep pattern = 
    return . unlines . filter isMatch . lines
    where regex = mkRegex pattern
          isMatch line = case matchRegex regex line of
                              Nothing -> False
                              Just _ -> True

