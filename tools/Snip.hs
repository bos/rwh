-- Read a pile of source files, and write snippets from each into new
-- files.  For example, if we read "Foo.hs" and it contains snippets
-- named "bar" and "quux", we'll write out new files named
-- "Foo__bar.hs" and "Foo__quux.hs".

module Main where

import Control.Monad (forM_, liftM)
import Snippet (Snippet(..), parseSnippets)
import System.Environment (getArgs)
import System.FilePath (splitExtension)
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()

main = getArgs >>= snipFiles

snipFiles :: [FilePath] -> IO ()

snipFiles names = forM_ names $ \fileName -> do
    snips <- parseSnippets `liftM` B.readFile fileName
    let (name, ext) = splitExtension fileName
    forM_ snips $ \(Snippet snip content) ->
        B.writeFile (name ++ "__" ++ B.unpack snip ++ ext) content
