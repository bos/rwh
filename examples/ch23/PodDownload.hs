{-- snippet all --}
-- ch23/PodDownload.hs

module PodDownload where
import PodTypes
import PodDB
import PodParser
import Network.HTTP
import System.IO
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe
import Network.URI

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadURL :: String -> Either String String
downloadURL url =
    do resp <- simpleHTTP request
       case rep of
         Left x -> Left $ "Error connecting: " ++ show x
         Right r -> 
             case rspCode r of
               (2,_,_) -> Right (rspBody r)
               _ -> Left (show r)
    where request = Request {rqURI = url,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}

{- | Update the podcast in the database. -}
updatePodcast :: Connection -> Podcast -> IO ()
updatePodcast dbh pc =
    do resp <- downloadURL (castURL pc)
       case resp of
         Left x -> putStrLn x
         Right doc -> updateDB doc

    where request = Request {rqURI = castURL,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          updateDB doc = 
              do mapM_ (addEpisode dbh) episodes
                 commit dbh
              where feed = parse doc (castURL pc)
                    episodes = map (item2ep pc) (items feed)

{- | Downloads an episode, returning a String representing
the filename it was placed into, or Nothing on error. -}
getEpisode :: Connection -> Episode -> IO (Maybe String)
getEpisode dbh ep =
    do resp <- downloadURL (epURL ep)
       case resp of
         Left x -> do putStrLn x
                      return Nothing
         Right doc -> 
             do file <- openBinaryFile filename WriteMode
                hPutStr file doc
                hClose file
                updateEpisode dbh (ep {epDone = True})
                commit dbh
                return (Right filename)
          -- This function ought to apply an extension based on the filetype
    where filename = "pod." ++ (show . castId . epCast $ ep) ++ "." ++ 
                     (show (epId ep)) ++ ".mp3"
{-- /snippet all --}
