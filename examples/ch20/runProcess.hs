import System.Process
import Control.Concurrent

{- | Storage for the Handles that we will be passing about -}
data Handles = Handles {
    inp :: Handle,
    out :: Handle,
    err :: Handle,
    getExitCode :: IO ExitCode}
    deriving (Eq, Show)

type Command = (String, [String])

(-|-) :: Handles -> Handles -> IO Handles
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

