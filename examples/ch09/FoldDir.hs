import ControlledVisit (Info(..), getInfo, getUsefulContents, isDirectory)

import Control.Monad (liftM)
import Data.Char (toLower)
import System.FilePath ((</>), takeBaseName, takeExtension)

{-- snippet Iterate --}
data Iterate seed = Done    { unwrap :: seed }
                  | Skip    { unwrap :: seed }
                  | Recurse { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed
{-- /snippet Iterate --}

{-- snippet foldTree --}
foldTree :: Iterator a -> a -> FilePath -> IO a

foldTree iter seed path = unwrap `liftM` fold seed path
  where
    fold seed path = getUsefulContents path >>= walk seed
      where
        walk seed (name:names) = do
          let path' = path </> name
          info <- getInfo path'
          case iter seed info of
            done@(Done _) -> return done
            Skip seed' -> walk seed' names
            Recurse seed' ->
                if isDirectory info
                then do next <- fold seed' path'
                        case next of
                          done@(Done _) -> return done
                          seed'' -> walk (unwrap seed'') names
                else walk seed' names
        walk seed _ = return (Recurse seed)
{-- /snippet foldTree --}

{-- snippet atMostThreePictures --}
atMostThreePictures :: Iterator [FilePath]

atMostThreePictures paths info =
    if isDirectory info && takeBaseName path == ".svn"
    then Skip paths
    else if extension `elem` ["jpg", "png"] && length paths' == 3
         then Done paths'
         else Recurse paths'
  where extension = map toLower (takeExtension path)
        path = infoPath info
        paths' = path : paths
{-- /snippet atMostThreePictures --}

{-- snippet countDirectories --}
countDirectories count info =
    Recurse (if isDirectory info
             then count + 1
             else count)
{-- /snippet countDirectories --}

{-- snippet secondTake --}
secondTake paths info =
    case undefined of
    _ | isDirectory info && takeBaseName path == ".svn"
          -> Skip paths
      | extension `elem` ["jpg", "png"] && length paths' == 3
          -> Done paths'
      | otherwise
          -> Recurse paths'
  where extension = map toLower (takeExtension path)
        path = infoPath info
        paths' = path : paths
{-- /snippet secondTake --}
