{-- snippet imports --}
import Control.Monad (filterM)
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
{-- /snippet imports --}
import System.FilePath (takeExtension)
{-- snippet simpleFileSize --}
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

simpleFileSize :: FilePath -> IO Integer

simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size
{-- /snippet simpleFileSize --}

{-- snippet getFileSize --}
getFileSize path = handle (const (return Nothing)) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)
{-- /snippet getFileSize --}

{-- snippet saferFileSize --}
saferFileSize path = handle (\_ -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)
{-- /snippet saferFileSize --}

{-- snippet Predicate --}
type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> ClockTime     -- last modified
               -> Bool
{-- /snippet Predicate --}

{-- snippet betterFind --}
getFileSize :: FilePath -> IO (Maybe Integer)

betterFind :: Predicate -> FilePath -> IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)
{-- /snippet betterFind --}

{-- snippet myTest --}
myTest name _ (Just size) _ =
    takeExtension name == ".c" && size > 1048576
myTest _ _ _ _ = False
{-- /snippet myTest --}
