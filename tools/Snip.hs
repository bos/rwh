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
import System.FilePath (joinPath, splitDirectories, takeExtension)
import System.IO (putStrLn)
import Util (baseName, suffix)
-- Don't use lazy ByteStrings, due to the bug in "lines".
import qualified Data.ByteString.Char8 as B

programListing :: String -> B.ByteString -> B.ByteString
               -> [(String, B.ByteString)]

programListing modName snipName body =
    [(tag ++ ".code",
      B.concat [B.pack "<![CDATA[", rstrip body, B.pack "]]>"]),
     (tag ++ ".noid", B.pack ("<programlisting>" ++
                              "&" ++ tag ++ ".code;" ++
                              "</programlisting>")),
     (tag, B.pack ("<programlisting id=" ++ show tag ++ ">" ++
                   "&" ++ tag ++ ".code;" ++
                   "</programlisting>"))]
  where tag = modName ++ ':' : B.unpack snipName
        rstrip = B.reverse . B.dropWhile isSpace . B.reverse

markers :: FilePath -> (B.ByteString, B.ByteString)
markers path =
    case suffix path of
      "hs" -> (startHs, endHs)
      "lhs" -> (startHs, endHs)
      "c"  -> (startC, endC)
      "h"  -> (startC, endC)
      "cabal"  -> (startCabal, endCabal)
      "cpp"  -> (startC, endC)
      "java"  -> (startC, endC)
      "js"  -> (startC, endC)
      "py"  -> (startPy, endPy)
      s    -> error ("unknown file suffix" ++ show s)
  where startHs = B.pack "{-- snippet "
        endHs = B.pack "{-- /snippet "
        startC = B.pack "/** snippet "
        endC = B.pack "/** /snippet "
        startCabal = B.pack "-- snippet "
        endCabal = B.pack "-- /snippet "
        startPy = B.pack "## snippet "
        endPy = B.pack "## /snippet "

snipFile :: FilePath -> FilePath -> IO ()

snipFile tgtDir fileName = do
    let (start, end) = markers fileName
    snips <- parseSnippets start end `liftM` B.readFile fileName
    let name = baseName fileName
        sfx = takeExtension fileName
    forM_ snips $ \(Snippet snip content) -> do
        let fixie | sfx == ".hs" =
                      B.append (B.pack $ "-- file: " ++ shortName ++ "\n")
                               content
                  | otherwise    = content
            shortName = joinPath . reverse . take 2 . reverse . splitDirectories $ fileName
        forM_ (programListing name snip fixie) $ \(entity, listing) -> do
            let outName = entity ++ ".xml"
            putStrLn ("<!ENTITY " ++ entity ++ " SYSTEM " ++ show outName ++ ">")
            B.writeFile (tgtDir ++ '/' : outName) listing

main :: IO ()

main = do
    (tgtDir:args) <- getArgs
    createDirectoryIfMissing False tgtDir
    mapM_ (snipFile tgtDir) args
