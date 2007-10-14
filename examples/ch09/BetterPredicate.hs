{-- snippet imports --}
import Control.Monad (filterM)
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
{-- /snippet imports --}
{-- snippet getFileSize --}
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

getFileSize path = handle (const (return Nothing)) $
  bracket (openFile path ReadMode) hClose ((Just `fmap`) . hFileSize)
{-- /snippet getFileSize --}

{-- snippet simpleFileSize --}
simpleFileSize :: FilePath -> IO Integer

simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size
{-- /snippet simpleFileSize --}

{-- snippet saferFileSize --}
saferFileSize path = bracket (openFile path ReadMode) hClose hFileSize
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
