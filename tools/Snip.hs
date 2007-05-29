-- Read a pile of source files, and write snippets from each into new
-- files.  For example, if we read "Foo.hs" and it contains snippets
-- named "bar" and "quux", we'll write out new files named
-- "Foo__bar.hs" and "Foo__quux.hs".

module Main where

import Control.Monad (forM_, liftM)
import Data.Char (isSpace)
import Snippet (Snippet(..), parseSnippets)
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import System.IO (putStrLn)
-- Don't use lazy ByteStrings, due to the bug in "lines".
import qualified Data.ByteString.Char8 as B

programListing :: String -> B.ByteString -> B.ByteString
               -> (String, B.ByteString)

programListing mod snipName body =
    (tag, B.concat [header, rstrip body, footer])
  where header = B.pack ("<programlisting id=" ++ show tag ++ ">\n<![CDATA[\n")
        tag = mod ++ ':' : B.unpack snipName
        footer = B.pack "\n]]>\n</programlisting>\n"
        rstrip = B.reverse . B.dropWhile isSpace . B.reverse

snipFiles :: [FilePath] -> IO ()

snipFiles names = forM_ names $ \fileName -> do
    snips <- parseSnippets `liftM` B.readFile fileName
    let name = takeFileName fileName
    forM_ snips $ \(Snippet snip content) -> do
        let (entity, listing) = programListing name snip content
            outName = entity ++ ".xml"
        putStrLn ("<!ENTITY " ++ entity ++ " SYSTEM " ++ show outName ++ ">")
        B.writeFile outName listing

main :: IO ()

main = getArgs >>= snipFiles
