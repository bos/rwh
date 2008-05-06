{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module WebApp
    (
      App
    , runApp
    , ClientError(..)
    , Handler
    , HttpRequest(..)
    , HttpResponse(..)
    , Request(..)
    , ok
    , clientError
    , dispatch
    , serverLoop
    , url
    , respStatus
    ) where

import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Monad.State
import qualified System.IO
import Network (PortID(..), accept, listenOn, sClose, withSocketsDo)
import Control.Exception (bracket, finally)
import System.IO (Handle)

import MonadHandle
import ServerParse
import URLParser


ok :: Monad m => String -> m HttpResponse
ok = return . RespSuccess ["Content-Type: application/json"]

clientError :: Monad m => ClientError -> String -> m HttpResponse
clientError kind = return . RespClientError kind ["Content-Type: text/plain"]

type Handler s = HttpRequest -> App s HttpResponse

data ClientError = BadRequest
                 | NotFound
                   deriving (Eq, Ord, Show)

data HttpResponse =
    RespSuccess {
      respHeaders :: [String]
    , respBody :: String
    } |
    RespClientError {
      respClientError_ :: ClientError
    , respHeaders :: [String]
    , respBody :: String
    } deriving (Eq, Ord, Show)

respStatus :: HttpResponse -> String
respStatus (RespSuccess _ _) = "200 OK"
respStatus (RespClientError BadRequest _ _) = "400 Bad Request"
respStatus (RespClientError NotFound _ _) = "404 Not Found"


newtype App s a = App (ReaderT Request (StateT s IO) a)
    deriving (Functor, Monad, MonadIO, MonadReader Request,
              MonadState s)

runApp :: s -> Request -> App s a -> IO (a, s)
runApp st req (App a) = runStateT (runReaderT a req) st

serverLoop :: s -> App s () -> Int -> IO ()
serverLoop st serv port = liftIO . withSocketsDo $
  bracket (listenOn . PortNumber . fromIntegral $ port) sClose $ \sock -> do
  putStrLn $ "listening on port " ++ show port
  forever $ do
    (handle, clientHost, clientPort) <- accept sock
    putStrLn $ "connect from " ++ show (clientHost, clientPort)
    let req = Request { reqClient = show clientHost
                      , reqHandle = handle }
    forkIO $ finally (runApp st req serv >> return ()) (hClose handle)

data Request = Request {
      reqClient :: String
    , reqHandle :: Handle
    }

url :: (Method -> Bool) -> URLParser (Handler s) -> HttpRequest
    -> Maybe (Handler s)
url methOK p req = do
  guard . methOK $ httpMethod req
  case parse p "" (httpURL req) of
    Left err -> fail (show err)
    Right h -> return h

dispatch :: [HttpRequest -> Maybe (Handler s)] -> HttpRequest
         -> App s HttpResponse
dispatch hs req = do
  case map ($req) hs of
    (Just f:_) -> f req
    _ -> clientError NotFound "not found"

instance MonadIO m => MonadHandle System.IO.Handle m where
    openFile path mode = liftIO $ System.IO.openFile path mode
    hPutStr h s = liftIO $ System.IO.hPutStr h s
    hClose = liftIO . System.IO.hClose
    hGetContents = liftIO . System.IO.hGetContents
    hPutStrLn h s = liftIO $ do
                      System.IO.hPutStr h s
                      System.IO.hPutStr h "\r\n"
