{-- snippet all --}
-- ch23/PodDB.hs

module PodDb where

import Database.HDBC
import Database.HDBC.Sqlite3
import PodTypes

-- | Initialize DB and return database Connection
connect :: String -> IO Connection
connect fp =
    do dbh <- connectSqlite3 fp
       prepDB dbh
       return dbh

{- | Prepare the database for our data.

We create two tables and ask the database engine to verify some pieces
of data consistency for us:

* castid and epid both are unique primary keys and must never be duplicated
* castURL also is unique
* In the spidoes table, for a given podcast (epcastid), there must be only
  one instance of each given URL or episode ID
-}
prepDB :: Connection -> IO ()
prepDB dbh =
    do tables <- getTables dbh
       if (sort tables) /= ["podcasts", "episodes"] then
           do run dbh "CREATE TABLE podcasts (\
                       \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \castURL TEXT NOT NULL UNIQUE)" []
              run dbh "CREATE TABLE episodes (\
                       \epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \epcastid INTEGER NOT NULL,\
                       \epurl TEXT NOT NULL,\
                       \epdone INTEGER NOT NULL,\
                       \UNIQUE(epcastid, epurl),\
                       \UNIQUE(epcastid, epid))" []
              commit dbh

{- | Adds a new podcast to the database.  Ignores the castid on the
incoming podcast, and returns a new object with the castid populated.

An attempt to add a podcast that already exists is an error. -}
addPodcast :: Connection -> Podcast -> IO Podcast
addPodcast dbh podcast = 
    handleSql errorHandler $
      do -- Insert the castURL into the table.  The database
         -- will automatically assign a cast ID.
         run dbh "INSERT INTO podcasts (castURL) VALUES (?)"
             [toSql (castURL podcast)]
         -- Find out the castID for the URL we just added.
         r <- quickQuery' dbh "SELECT castid FROM podcasts WHERE castURL = ?"
              [toSql (castURL podcast)]
         case r of
           [[x]] -> return $ podcast {castid = fromSql x}
           y -> fail $ "addPodcast: unexpected result: " ++ show y
    where errorHandler e = 
              fail $ "Error adding podcast; does this URL already exist?\n"
                     ++ show e

{- | Adds a new episode to the database. 

Since this is done by automation, instead of by user request, we will
simply ignore requests to add duplicate episodes.  This way, when we are
processing a feed, each URL encountered can be fed to this function,
without having to first look it up in the DB.

Also, we generally won't care about the new ID here, so don't bother
fetching it. -}
addEpisode :: Connection -> Episode -> IO ()
addEpisode dbh ep =
    run dbh "INSERT OR IGNORE INTO episodes (epCastId, epURL, epDone) \
                \VALUES (?, ?, ?)"
                [toSql (epCastId ep), toSql (epURL ep),
                 toSql (epDone ep)]
       
{- | Modifies an existing podcast.  Looks up the given podcast by
ID and modifies the database record to match the passed Podcast. -}
updatePodcast :: Connection -> Podcast -> IO ()
updatePodcast dbh podcast =
    run dbh "UPDATE podcasts SET castURL = ? WHERE castId = ?" 
            [toSql (castURL podcast), toSql (castId podcast)]

{- | Modifies an existing episode.  Looks it up by ID and modifies the
database record to match the given episode. -}
updateEpisode :: Connection -> Episode -> IO ()
updateEpisode dbh episode =
    run dbh "UPDATE episodes SET epCastId = ?, epURL = ?, epDone = ? \
             \WHERE epId = ?"
             [toSql (epCastId episode),
              toSql (epURL episode),
              toSql (epDone episode),
              toSql (epId episode)]

{- | Remove a podcast.  First removes any episodes that may exist
for this podcast. -}
removePodcast :: Connection -> Podcast -> IO ()
removePodcast dbh podcast =
    run dbh "DELETE FROM episodes WHERE epcastid = ?" 
        [toSql (castId podcast)]
    run dbh "DELETE FROM podcasts WHERE castid = ?"
        [toSql (castId podcast)]

{- | Gets a list of all podcasts. -}
getPodcasts :: Connection -> IO [Podcast]
getPodcasts dbh =
    do res <- quickQuery' dbh 
              "SELECT castid, casturl FROM podcasts ORDER BY castid" []
       return (map convPodcastRow res)

{- | Get a particular podcast.  Nothing if the ID doesn't match, or
Just Podcast if it does. -}
getPodcast :: Connection -> Integer -> IO (Maybe Podcast)
getPodcast dbh wantedId =
    do res <- quickQuery' dbh 
              "SELECT castid, caturl FROM podcasts WHERE castid = ?"
              [toSql wantedId]
       case res of
         [x] -> return (Just (convPodcastRow res))
         [] -> return Nothing
         x -> fail $ "Really bad error; more than one podcast with ID"

{- | Convert the result of a SELECT into a Podcast record -}
convPodcastRow :: [SqlValue] -> Podcast
convPodcastRow [svId, svURL] =
    Podcast {castId = fromSql svId,
             castURL = fromSql svURL}
convPodcastRow x = error $ "Can't convert podcast row " ++ show x

{- | Get all episodes for a particular podcast. -}
getPodcastEpisodes :: Connection -> Podcast -> IO [Episode]
getPodcastEpisodes dbh pc =
    do r <- quickQuery' dbh
            "SELECT epId, epURL, epDone FROM episodes WHERE epCastId = ?"
            [toSql (castId pc)]
       return (map convEpisodeRow r)
    where convEpisodeRow [svId, svURL, svDone] =
              Episode {epId = fromSql svId, epURL = fromSql svURL,
                       epDone = fromSql svDone, epCastId = castId pc}

{-- /snippet all --}