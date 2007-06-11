-- Read a pile of source files, and write snippets from each into new
-- files.  For example, if we read "Foo.hs" and it contains snippets
-- named "bar" and "quux", we'll write out new files named
-- "Foo__bar.hs" and "Foo__quux.hs".

module Main where

import Control.Monad (forM_, liftM)
import Data.Char (isSpace)
import Snippet (Snippet(..), parseSnippets)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (putStrLn)
-- Don't use lazy ByteStrings, due to the bug in "lines".
import qualified Data.ByteString.Char8 as B

-- We'd prefer to use the filepath library's version of this function,
-- but ghc 6.6.1 doesn't seem to be widely available yet.  Sigh.
takeFileName :: FilePath -> String
takeFileName = reverse . takeWhile (/='/') . reverse

programListing :: String -> B.ByteString -> B.ByteString
               -> (String, B.ByteString)

programListing mod snipName body =
    (tag, B.concat [header, rstrip body, footer])
  where header = B.pack ("<programlisting id=" ++ show tag ++ ">\n<![CDATA[\n")
        tag = mod ++ ':' : B.unpack snipName
        footer = B.pack "\n]]>\n</programlisting>\n"
        rstrip = B.reverse . B.dropWhile isSpace . B.reverse

snipFile :: FilePath -> FilePath -> IO ()

snipFile tgtDir fileName = do
    snips <- parseSnippets `liftM` B.readFile fileName
    let name = takeFileName fileName
    forM_ snips $ \(Snippet snip content) -> do
        let (entity, listing) = programListing name snip content
            outName = entity ++ ".xml"
        putStrLn ("<!ENTITY " ++ entity ++ " SYSTEM " ++ show outName ++ ">")
        B.writeFile (tgtDir ++ '/' : outName) listing

main :: IO ()

main = do
    (tgtDir:args) <- getArgs
    createDirectoryIfMissing False tgtDir
    mapM_ (snipFile tgtDir) args
