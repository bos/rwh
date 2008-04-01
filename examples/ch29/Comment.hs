{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module Comment
    (
      ElementID(..)
    , Element(..)
    , Comment(..)
    , main
    ) where

import ApplicativeParsec
import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (bracket, finally)
import Control.Monad (forever, guard, join, liftM, liftM2, liftM3)
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Function (on)
import Data.List (foldl')
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
  where fetch k n = map ((,) =<< k) . fst . readAll <$> readFile n
        cmt m (k, v) = M.insertWith' (++) k [v] m
        ch m (k, v) = M.insertWith' (++) (eltChapter v) [k] m
  
main :: IO ()
main = do
  let e = newTVarIO M.empty
  st <- liftM3 AppState e e e
  atomically =<< updateMaps st
  serverLoop st serve 12345
  return ()

serverLoop :: AppState -> (Handle -> H ()) -> Int -> IO ()
serverLoop st serv port = liftIO . withSocketsDo $
  bracket (listenOn . PortNumber . fromIntegral $ port) sClose $ \sock -> do
  putStrLn $ "listening on port " ++ show port
  forever $ do
    (handle, clientHost, clientPort) <- accept sock
    putStrLn $ "connect from " ++ show (clientHost, clientPort)
    forkIO $ finally (runH st (serv handle) >> return ()) (hClose handle)

class Response a where
    respHandle :: a -> Handle

instance Response Handle where respHandle = id
instance Response HttpRequest where respHandle = reqHandle

hPutLine :: Response a => a -> String -> IO ()
hPutLine r s = do
  let h = respHandle r
  hPutStr h s
  hPutStr h "\r\n"

hPut :: Response a => a -> String -> IO ()
hPut r s = hPutStr (respHandle r) s

ok :: Monad m => String -> m HttpResponse
ok = return . RespSuccess ["Content-Type: application/json"]

clientError :: Monad m => ClientError -> String -> m HttpResponse
clientError kind = return . RespClientError kind ["Content-Type: text/plain"]

notSep :: CharParser () Char
notSep = noneOf "/?#\r\n"

type Handler = HttpRequest -> H HttpResponse

part :: CharParser () String
part = many1 notSep

class URLPart a b | a -> b where
    urlPart :: a -> CharParser () b

instance URLPart Char Char where
    urlPart c = char c

instance URLPart String String where
    urlPart s = string s

instance URLPart (CharParser () a) a where
    urlPart p = p

(/>) :: (URLPart a _z, URLPart b c) => a -> b -> CharParser () c
a /> b = urlPart a *> char '/' *> urlPart b
infixl 4 />

(</) :: (URLPart a c, URLPart b _z) => a -> b -> CharParser () c
a </ b = urlPart a <* char '/' <* urlPart b
infixl 4 </

data ClientError = BadRequest
                 | NotFound
                   deriving (Eq, Ord, Show)

newtype H a = H (StateT AppState IO a)
    deriving (Functor, Monad, MonadIO, MonadState AppState)

runH :: AppState -> H a -> IO (a, AppState)
runH st (H a) = runStateT a st

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

q :: CharParser () a -> CharParser () a
q a = a

query :: CharParser () [(String, Maybe String)]
query = char '?' *> p_query <|> [] <$ eof 

handlers :: [HttpRequest -> Maybe Handler]
handlers = [
   url (==Get) (chCount <$> ("/chapter" /> part </ "count"))
 , url (==Post) (cmtSubmit . ElementID <$> ("/element" /> part </ "submit"))
 ]

dispatch :: [HttpRequest -> Maybe Handler] -> HttpRequest
         -> H HttpResponse
dispatch hs req = do
  case map ($req) hs of
    (Just f:_) -> f req
    _ -> clientError NotFound "not found"

chCount :: String -> HttpRequest -> H HttpResponse
chCount ch _ = do
    st <- get
    (cmts, chs) <- liftIO . atomically $ liftM2 (,) ((readTVar . appComments) $ st)
                                           ((readTVar . appChapters) $ st)
    let go a elt = a + maybe 0 length (M.lookup elt cmts)
    case M.lookup ch chs of
      Nothing -> clientError NotFound "chapter not found"
      Just elts -> ok . show $ foldl' go 0 elts
  
joinLookup k kvs = join (lookup k kvs)

cmtSubmit :: ElementID -> HttpRequest -> H HttpResponse
cmtSubmit elt req = do
  st <- get
  liftIO $ atomically $ do
  elts <- readTVar . appElements $ st
  case M.lookup elt elts of
    Nothing -> clientError NotFound "element not found"
    Just _ ->
      case parse p_query "" <$> reqBody req of
          Nothing -> clientError BadRequest "empty comment"
          Just (Left err) -> clientError BadRequest "malformed string"
          Just (Right kvs) -> do
            let tv = appComments st
                mcmt = Comment elt <$> joinLookup "body" kvs
                                   <*> joinLookup "submitter" kvs
                                   <*> joinLookup "url" kvs
                                   <*> pure "cmtIP"
                                   <*> pure "cmtDate"
                                   <*> pure False
                                   <*> pure False
            case mcmt of
              Nothing -> clientError BadRequest "malformed request"
              Just cmt -> do
                m <- readTVar tv
                case M.lookup elt m of
                  Nothing -> do writeTVar tv $ M.insert elt [cmt] m
                                ok "comment added"
                  Just cmts ->
                    if any ((cmtComment cmt ==) . cmtComment) cmts
                    then ok "comment already present"
                    else do writeTVar tv $ M.insertWith' (++) elt [cmt] m
                            ok "comment added"

url :: (Method -> Bool) -> CharParser () Handler -> HttpRequest -> Maybe Handler
url methOK p req = do
  guard . methOK $ reqMethod req
  case parse p "" (reqURL req) of
    Left err -> fail (show err)
    Right h -> return h

serve :: Handle -> H ()
serve h = do
  input <- liftIO $ hGetContents h
  resp <- case parse p_request "" input of
    Left err -> clientError BadRequest ("Bad request " ++ show err)
    Right preq -> dispatch handlers (preq h)
  let putLine = liftIO . hPutLine h
      put = liftIO . hPut h
  putLine $ "HTTP/1.1 " ++ respStatus resp
  mapM_ putLine (respHeaders resp)
  putLine ""
  case respBody resp of
    [] -> return ()
    body -> do
      put body
      if last body /= '\n'
        then putLine ""
        else return ()
