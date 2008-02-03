{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Comment
    (
      ElementID(..)
    , Element(..)
    , Comment(..)
    ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Concurrent (forkIO)
import Control.Exception (bracket, finally)
import Control.Monad (forever, liftM)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (MonadIO(..))
import qualified Data.Map as M
import Network (PortID(..), accept, listenOn, sClose, withSocketsDo)
import System.IO (Handle, hClose, hGetContents, hPutStr)

import JSON (JSON(..))
import ServerParse

newtype ElementID = ElementID {
      fromElementID :: String
    } deriving (Eq, Ord, Read, Show)

instance JSON ElementID where
    fromJValue = liftM ElementID . fromJValue
    toJValue = toJValue . fromElementID

data Element = Element {
      eltID :: ElementID
    , eltChapter :: String
    , eltSection :: String
    } deriving (Eq, Ord, Read, Show)

data Comment = Comment {
      cmtEltID :: ElementID
    , cmtComment :: String
    , cmtSubmitterName :: String
    , cmtSubmitterURL :: String
    , cmtIP :: String
    , cmtDate :: String
    , cmtReviewed :: Bool
    , cmtHidden :: Bool
    } deriving (Eq, Ord, Read, Show)

data SizedMap k e = SizedMap {
      smMap :: M.Map k e
    , origSize :: !Int
    }

data AppState = AppState {
      appElements :: TVar (M.Map ElementID Element)
    , appComments :: TVar (M.Map ElementID [Comment])
    }

newtype H a = H (ReaderT AppState IO a)
    deriving (Functor, Monad, MonadIO, MonadReader AppState)

main :: IO ()
main = do
  elts <- newTVarIO M.empty
  cmts <- newTVarIO M.empty
  let st = AppState elts cmts
  serverLoop (serve st) 12345

serverLoop :: (Handle -> IO ()) -> Int -> IO ()
serverLoop serve port = withSocketsDo $
  bracket (listenOn . PortNumber . fromIntegral $ port) sClose $ \sock ->
  forever $ do
    (handle, clientHost, clientPort) <- accept sock
    putStrLn $ "connect from " ++ show (clientHost, clientPort)
    forkIO $ finally (serve handle) (hClose handle)

hPutLine :: Handle -> String -> IO ()
hPutLine h s = hPutStr h s >> hPutStr h "\r\n"

serve :: AppState -> Handle -> IO ()
serve st h = do
  input <- hGetContents h
  case parse p_request "" input of
    Left err -> mapM_ (hPutLine h) [
                                 "HTTP/1.1 400 Bad Request"
                                , "Connection: close"
                                , ""
                                , "Bad request " ++ show err
                                ]
    Right req -> case reqType req of
                   "GET" -> doGet st req
                   "POST" -> doPost st req
                   _ -> fail "eep!"

doGet :: AppState -> HttpRequest -> IO ()
doGet st req = return ()

doPost :: AppState -> HttpRequest -> IO ()
doPost st req = return ()
