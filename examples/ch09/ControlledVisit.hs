import Control.Monad (filterM, forM, liftM)
import Data.List (partition)
import Data.Maybe (isJust)
import System.Directory (Permissions(..), getDirectoryContents,
                         getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

{-- snippet Info --}
data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe ClockTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
{-- /snippet Info --}

{-- snippet getInfo --}
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (const (return Nothing)) (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)
{-- /snippet getInfo --}

{-- snippet traverse.type --}
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
{-- /snippet traverse.type --}
{-- snippet traverse --}
traverse order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (not . (`elem` [".", ".."])) names
    contents <- mapM (getInfo . (path </>)) ("" : usefulNames)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]
  where isDirectory = maybe False searchable . infoPerms
{-- /snippet traverse --}

{-- snippet traverseVerbose --}
traverseVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (not . (`elem` [".", ".."])) names
    contents <- mapM getEntryName ("" : usefulNames)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
  where getEntryName name = getInfo (path </> name)
        isDirectory info = case infoPerms info of
                             Nothing -> False
                             Just perms -> searchable perms
        recurse info = do
            if isDirectory info && infoPath info /= path
                then traverseVerbose order (infoPath info)
                else return [info]
{-- /snippet traverseVerbose --}
