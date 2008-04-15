{-- snippet countEntriesTrad --}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM_, liftM)
import Control.Monad.Writer

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- liftIO $ doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest
{-- /snippet countEntriesTrad --}

newtype Traversal a = Traversal {
      runT :: WriterT [(FilePath, Int)] IO a
    } deriving (Monad, MonadWriter [(FilePath, Int)], MonadIO)

runTraversal = runWriterT . runT

countEntriesT :: FilePath -> Traversal ()
countEntriesT path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO $ doesDirectoryExist newName
    when isDir $ countEntriesT newName

{-- snippet countEntries --}
countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO $ doesDirectoryExist newName
    when isDir $ countEntries newName
{-- /snippet countEntries --}
