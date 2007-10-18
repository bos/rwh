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
myTest path _ (Just size) _ =
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False
{-- /snippet myTest --}

{-- snippet InfoP --}
type InfoP a = FilePath         -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> ClockTime       -- last modified
             -> a

pathP :: InfoP FilePath
{-- /snippet InfoP --}

{-- snippet pathP --}
pathP path _ _ _ = path
{-- /snippet pathP --}

{-- snippet sizeP --}
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1
{-- /snippet sizeP --}

{-- snippet equalP --}
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k
{-- /snippet equalP --}

{-- snippet equalP2 --}
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k
{-- /snippet equalP2 --}

{-- snippet liftP2 --}
liftP2 :: (a -> a -> b) -> InfoP a -> a -> InfoP b
liftP2 f g k w x y z = g w x y z `f` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP2 (>)
lesserP = liftP2 (<)
{-- /snippet liftP2 --}

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP f g w x y z = f w x y z && g w x y z
