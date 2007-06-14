module Glob (namesMatching) where

import Control.Exception (handle)
import Control.Monad (forM, liftM)
import GlobRegex (matchesGlob)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

{-- snippet type --}
namesMatching :: String -> IO [FilePath]
{-- /snippet type --}

namesMatching pat
  | isMagical pat = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          globPattern curDir baseName
      (dirName, baseName) -> do
          dirs <- if isMagical dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let glob = if isMagical baseName then globPattern else globPlain
          pathNames <- forM dirs $ \dir -> do
                           baseNames <- glob dir baseName
                           return (map (dir </>) baseNames)
          return (concat pathNames)
  | otherwise = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])

{-- snippet type --}
isMagical :: String -> Bool
isMagical = any (`elem` "[*?")
{-- /snippet type --}

globPlain :: FilePath -> String -> IO [String]
globPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])

globPattern :: FilePath -> String -> IO [String]
globPattern dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return [])) $ do
        names <- getDirectoryContents dirName'
        let names' = if notHidden pat
                     then filter notHidden names
                     else names
        return (filter (`matchesGlob` pat) names')

{-- snippet notHidden --}
notHidden :: String -> Bool
notHidden name = take 1 name /= "."
{-- /snippet notHidden --}

{-- snippet doesNameExist --}
doesNameExist :: FilePath -> IO Bool

doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
{-- /snippet doesNameExist --}

