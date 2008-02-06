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
import Control.Concurrent (forkIO)
import Control.Exception (bracket, finally)
import Control.Monad (forever, guard, liftM, liftM2, liftM3)
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
  serverLoop (serve st) 12345

serverLoop :: (Handle -> IO ()) -> Int -> IO ()
serverLoop serv port = withSocketsDo $
  bracket (listenOn . PortNumber . fromIntegral $ port) sClose $ \sock -> do
  putStrLn $ "listening on port " ++ show port
  forever $ do
    (handle, clientHost, clientPort) <- accept sock
    putStrLn $ "connect from " ++ show (clientHost, clientPort)
    forkIO $ finally (serv handle) (hClose handle)

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

ok :: String -> IO HttpResponse
ok = return . RespSuccess ["Content-Type: application/json"]

clientError :: ClientError -> String -> IO HttpResponse
clientError kind = return . RespClientError kind ["Content-Type: text/plain"]

notSep :: CharParser () Char
notSep = noneOf "/?#\r\n"

type Handler = AppState -> HttpRequest -> IO HttpResponse
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
   url (==Get) (chAll <$> ("/chapter" /> part </ "all"))
 , url (==Get) (chCount <$> ("/chapter" /> part </ "count"))
 ]

dispatch :: [HttpRequest -> Maybe Handler] -> AppState -> HttpRequest
         -> IO HttpResponse
dispatch hs st req = do
  case map ($req) hs of
    (Just f:_) -> f st req
    _ -> clientError NotFound "not found"

chAll :: String -> AppState -> HttpRequest -> IO HttpResponse
chAll ch st _ = do
    (cmts, chs) <- atomically $ liftM2 (,) (readTVar . appComments $ st)
                                           (readTVar . appChapters $ st)
    let go a elt = case M.lookup elt cmts of
                     Just cs -> a + length cs
                     Nothing -> a
    case M.lookup ch chs of
      Nothing -> clientError NotFound "not found"
      Just elts -> ok . show $ foldl' go 0 elts
  

chCount :: String -> AppState -> HttpRequest -> IO HttpResponse
chCount s st req = undefined

url :: (Method -> Bool) -> CharParser () Handler -> HttpRequest -> Maybe Handler
url methOK p req = do
  guard . methOK $ reqMethod req
  case parse p "" (reqURL req) of
    Left err -> fail (show err)
    Right h -> return h

serve :: AppState -> Handle -> IO ()
serve st h = do
  input <- hGetContents h
  resp <- case parse p_request "" input of
    Left err -> clientError BadRequest ("Bad request " ++ show err)
    Right preq -> dispatch handlers st (preq h)
  hPutLine h $ "HTTP/1.1 " ++ respStatus resp
  mapM_ (hPutLine h) (respHeaders resp)
  hPutLine h ""
  case respBody resp of
    [] -> return ()
    body -> do
      hPut h body
      if last body /= '\n'
        then hPutLine h ""
        else return ()
