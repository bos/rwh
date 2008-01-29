-- ch31/syslogclient.hs

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc =
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       procMessages sock
    where procMessages sock =
              do (msg, _, addr) <- recvFrom sock 1024
                 handlerfunc addr msg
                 procMessages sock

plainHandler addr msg = 
    putStrLn $ "From " ++ show addr ++ ": " ++ msg