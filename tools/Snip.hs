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
import Util (baseName, suffix)
-- Don't use lazy ByteStrings, due to the bug in "lines".
import qualified Data.ByteString.Char8 as B

programListing :: String -> B.ByteString -> B.ByteString
               -> [(String, B.ByteString)]

programListing modName snipName body =
    [(tag ++ ".code",
      B.concat [B.pack "<![CDATA[\n", rstrip body, B.pack "\n]]>\n"]),
     (tag ++ ".noid", B.pack ("<programlisting>\n" ++
                              "&" ++ tag ++ ".code;\n" ++
                              "</programlisting>\n")),
     (tag, B.pack ("<programlisting id=" ++ show tag ++ ">\n" ++
                   "&" ++ tag ++ ".code;\n" ++
                   "</programlisting>\n"))]
  where tag = modName ++ ':' : B.unpack snipName
        rstrip = B.reverse . B.dropWhile isSpace . B.reverse

markers :: FilePath -> (B.ByteString, B.ByteString)
markers path =
    case suffix path of
      "hs" -> (startHs, endHs)
      "c"  -> (startC, endC)
      s    -> error ("unknown file suffix" ++ show s)
  where startHs = B.pack "{-- snippet "
        endHs = B.pack "{-- /snippet "
        startC = B.pack "/** snippet "
        endC = B.pack "/** /snippet "

snipFile :: FilePath -> FilePath -> IO ()

snipFile tgtDir fileName = do
    let (start, end) = markers fileName
    snips <- parseSnippets start end `liftM` B.readFile fileName
    let name = baseName fileName
    forM_ snips $ \(Snippet snip content) -> do
        forM_ (programListing name snip content) $ \(entity, listing) -> do
            let outName = entity ++ ".xml"
            putStrLn ("<!ENTITY " ++ entity ++ " SYSTEM " ++ show outName ++ ">")
            B.writeFile (tgtDir ++ '/' : outName) listing

main :: IO ()

main = do
    (tgtDir:args) <- getArgs
    createDirectoryIfMissing False tgtDir
    mapM_ (snipFile tgtDir) args
