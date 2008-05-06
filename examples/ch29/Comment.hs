{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module Comment
    (
      ElementID(..)
    , Element(..)
    , Comment(..)
    , runServers
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (forever, guard, join, liftM, liftM2, liftM3)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Function (on)
import Data.List (foldl')
import qualified Data.Map as M
import Network (PortID(..), accept, listenOn, sClose, withSocketsDo)
import qualified System.IO
import System.IO (Handle)

import MonadHandle
import URLParser
import JSONClass
import PrettyJSONClass
import ServerParse

newtype ElementID = ElementID {
      fromElementID :: String
    } deriving (Eq, Ord, Read, Show)

instance JSON ElementID where
    fromJValue = liftM ElementID . fromJValue
    toJValue = toJValue . fromElementID

data Element = Element {
      eltID :: ElementID
    , eltChapter :: ChapterID
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

instance JSON Comment where
    toJValue c = JObject . JObj $ [
                        ("element", toJValue $ cmtEltID c)
                      , ("comment", toJValue $ cmtComment c)
                      , ("submitter", toJValue $ cmtSubmitterName c)
                      , ("url", toJValue $ cmtSubmitterURL c)
                      , ("date", toJValue $ cmtDate c)
                      ]
    fromJValue _ = fail "not needed"

type ChapterID = String

data AppState = AppState {
      appElements :: TVar (M.Map ElementID Element)
    , appChapters :: TVar (M.Map ChapterID [ElementID])
    , appComments :: TVar (M.Map ElementID [Comment])
    }

readAll :: Read a => String -> ([a], String)
readAll s = case reads s of
              ((a,s'):_) -> case readAll s' of
                              ([], _) -> ([a], s')
                              (as, ss) -> (a:as, ss)
              _ -> ([], [])

updateMaps :: AppState -> IO (STM ())
updateMaps st = do
    cs <- fetch cmtEltID "comments.dat"
    es <- fetch eltID "elements.dat"
    putStrLn $ show (length es) ++ " elements, " ++
               show (length cs) ++ " comments"
    return $ do
      writeTVar (appElements st) $ M.fromList es
      writeTVar (appChapters st) $ foldl' ch M.empty es
      writeTVar (appComments st) $ foldl' cmt M.empty cs
  where fetch k n = do
                  putStrLn $ "reading " ++ n
                  map ((,) =<< k) . fst . readAll <$> readFile n
        cmt m (k, v) = M.insertWith' (++) k [v] m
        ch m (k, v) = M.insertWith' (++) (eltChapter v) [k] m
  
runServers :: IO ()
runServers = do
  let e = newTVarIO M.empty
  st <- liftM3 AppState e e e
  atomically =<< updateMaps st
  forkIO $ serverLoop st serve 12345
  forkIO $ serverLoop st reload 12346
  forever $ threadDelay maxBound

serverLoop :: AppState -> H () -> Int -> IO ()
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

ok :: Monad m => String -> m HttpResponse
ok = return . RespSuccess ["Content-Type: application/json"]

clientError :: Monad m => ClientError -> String -> m HttpResponse
clientError kind = return . RespClientError kind ["Content-Type: text/plain"]

type Handler = HttpRequest -> H HttpResponse

data ClientError = BadRequest
                 | NotFound
                   deriving (Eq, Ord, Show)

newtype App s a = App (ReaderT Request (StateT s IO) a)
    deriving (Functor, Monad, MonadIO, MonadReader Request,
              MonadState s)

type H a = App AppState a

runApp :: s -> Request -> App s a -> IO (a, s)
runApp st req (App a) = runStateT (runReaderT a req) st

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

handlers :: [HttpRequest -> Maybe Handler]
handlers = [
   url (==Get) (chCount <$> ("chapter" /> part </ "count" <* end))
 , url (==Get) (cmtSingle . ElementID <$> ("single" /> part <* end))
 , url (==Post) (cmtSubmit . ElementID <$> ("element" /> part </ "submit" <* end))
 ]

dispatch :: [HttpRequest -> Maybe Handler] -> HttpRequest
         -> H HttpResponse
dispatch hs req = do
  case map ($req) hs of
    (Just f:_) -> f req
    _ -> clientError NotFound "not found"

atomic :: MonadIO m => STM s -> m s
atomic = liftIO . atomically

chCount :: String -> HttpRequest -> H HttpResponse
chCount ch _ = do
    comments <- gets appComments
    chapters <- gets appChapters
    (cmts, chs) <- atomic $ liftM2 (,) (readTVar comments)
                                       (readTVar chapters)
    let go elt = (fromElementID elt, maybe 0 length (M.lookup elt cmts))
    case M.lookup ch chs of
      Nothing -> clientError NotFound "chapter not found"
      Just elts -> ok . jstring . JObj $ map go elts
  
cmtSingle :: ElementID -> HttpRequest -> H HttpResponse
cmtSingle elt _ = do
  comments <- (atomic . readTVar) =<< gets appComments
  case M.lookup elt comments of
    Nothing -> clientError NotFound "element not found"
    Just cmts -> ok . jstring . JAry $ cmts
  
joinLookup :: (Eq a) => a -> [(a, Maybe b)] -> Maybe b
joinLookup k kvs = join (lookup k kvs)

cmtSubmit :: ElementID -> HttpRequest -> H HttpResponse
cmtSubmit elt req = do
  st <- get
  client <- asks reqClient
  atomic $ do
  elts <- readTVar . appElements $ st
  case M.lookup elt elts of
    Nothing -> clientError NotFound "element not found"
    Just _ ->
      case parse p_query "" <$> httpBody req of
        Nothing -> clientError BadRequest "empty comment"
        Just (Left err) -> clientError BadRequest "malformed string"
        Just (Right kvs) -> do
          let mcmt = Comment elt <$> joinLookup "body" kvs
                                 <*> joinLookup "submitter" kvs
                                 <*> joinLookup "url" kvs
                                 <*> pure client
                                 <*> pure "cmtDate"
                                 <*> pure False
                                 <*> pure False
          case mcmt of
            Nothing -> clientError BadRequest "malformed request"
            Just cmt -> do
              let tv = appComments st
              m <- readTVar tv
              case M.lookup elt m of
                Nothing -> do writeTVar tv $ M.insert elt [cmt] m
                              ok "comment added"
                Just cmts ->
                  if any (on (==) cmtComment cmt) cmts
                  then ok "comment already present"
                  else do writeTVar tv $ M.insertWith' (++) elt [cmt] m
                          ok "comment added"

url :: (Method -> Bool) -> URLParser Handler -> HttpRequest -> Maybe Handler
url methOK p req = do
  guard . methOK $ httpMethod req
  case parse p "" (httpURL req) of
    Left err -> fail (show err)
    Right h -> return h

reload :: H ()
reload = do
  h <- asks reqHandle
  st <- get
  liftIO $ do
    hClose h
    updateMaps st >>= atomic

instance MonadIO m => MonadHandle System.IO.Handle m where
    openFile path mode = liftIO $ System.IO.openFile path mode
    hPutStr h s = liftIO $ System.IO.hPutStr h s
    hClose = liftIO . System.IO.hClose
    hGetContents = liftIO . System.IO.hGetContents
    hPutStrLn h s = liftIO $ do
                      System.IO.hPutStr h s
                      System.IO.hPutStr h "\r\n"

serve :: H ()
serve = do
  h <- asks reqHandle
  input <- hGetContents h
  resp <- case parse p_request "" input of
    Left err -> clientError BadRequest ("Bad request " ++ show err)
    Right preq -> dispatch handlers preq
  hPutStrLn h $ "HTTP/1.1 " ++ respStatus resp
  mapM_ (hPutStrLn h) (respHeaders resp)
  hPutStrLn h ""
  case respBody resp of
    [] -> return ()
    body -> do
      hPutStr h body
      if last body /= '\n'
        then hPutStrLn h ""
        else return ()
