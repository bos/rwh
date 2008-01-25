-- ch31/syslogclient.hs

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import SyslogTypes

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handler =
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo (defaultHints {addrFlags = [AI_PASSIVE]})
                    Nothing (Just port)
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       listen sock 5
       procMessages sock
    where procMessages sock =
              do (msg, _, addr) <- recvFrom sock 1024
                 putStrLn $ (show addr) ++ ": " ++ msg
                 procMessages sock

