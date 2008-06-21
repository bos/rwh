import Control.Monad (forM_)
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), dropExtension)

main = do
  let dir = "snippets"
  names <- filter (".xml" `isSuffixOf`) `fmap` getDirectoryContents dir
  forM_ names $ \name -> do
    let path = dir </> name
        prefix = dropExtension name
    putStrLn $ "<!ENTITY " ++ prefix ++ " SYSTEM \"" ++ path ++ "\">"
