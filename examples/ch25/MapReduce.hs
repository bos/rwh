import Data.Int (Int64)
import Control.Exception (bracket, finally)
import Control.Monad (forM, forM_, liftM)
import Control.Monad.Fix (fix)
import Control.Parallel (pseq)
import Control.Parallel.Strategies
import GHC.Conc (numCapabilities)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Environment (getArgs)
import System.IO
import Data.ByteString.Search.BoyerMoore (matchSL)

data ChunkSpec = CS {
      chunkOffset :: !Int64
    , chunkLength :: !Int64
    }

lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks numChunks path = do
  bracket (openFile path ReadMode) hClose $ \h -> do
    totalSize <- fromIntegral `liftM` hFileSize h
    let chunkSize = totalSize `div` fromIntegral numChunks
    flip fix 0 $ \findChunks offset -> do
      let newOffset = offset + chunkSize
      hSeek h AbsoluteSeek (fromIntegral newOffset)
      flip fix newOffset $ \loop off -> do
        eof <- hIsEOF h
        if eof
          then return [CS offset (totalSize - offset)]
          else do
            bytes <- LB.hGet h 4096
            case LB.elemIndex '\n' bytes of
              Just n -> do
                chunks@(c:_) <- findChunks (off + n + 1)
                let coff = chunkOffset c
                return (CS offset (coff - offset):chunks)
              Nothing -> loop (off + LB.length bytes)

chunkedRead :: (FilePath -> IO [ChunkSpec]) -> FilePath
            -> IO ([LB.ByteString], [Handle])
chunkedRead chunkfn path = do
  cxs <- chunkfn path
  chs <- forM cxs $ \spec -> do
    h <- openFile path ReadMode
    hSeek h AbsoluteSeek (fromIntegral (chunkOffset spec))
    chunk <- LB.take (chunkLength spec) `liftM` LB.hGetContents h
    return (chunk, h)
  return (unzip chs)

{-- snippet simpleMapReduce.type --}
simpleMapReduce
    :: (a -> b)      -- map function
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
{-- /snippet simpleMapReduce.type --}

{-- snippet simpleMapReduce --}
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc
{-- /snippet simpleMapReduce --}

{-- snippet mapReduce.type --}
mapReduce
    :: Strategy b    -- evaluation strategy for mapping
    -> (a -> b)      -- map function
    -> Strategy c    -- evaluation strategy for reduction
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
{-- /snippet mapReduce.type --}

{-- snippet mapReduce --}
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat
{-- /snippet mapReduce --}

withChunks :: (FilePath -> IO [ChunkSpec])
           -> ([LB.ByteString] -> a)
           -> FilePath
           -> IO a
withChunks chunkFunc process path = do
  (chunks, handles) <- chunkedRead chunkFunc path
  (return $! process chunks) `finally` mapM_ hClose handles

isInfixOf :: String -> LB.ByteString -> Bool
isInfixOf needle = not . null . matchSL (SB.pack needle)

main :: IO ()
main = do
  args <- getArgs
  let files = if null args
              then ["/home/bos/svnbook-access_log"]
              else args
  forM_ files $ \path -> do
    r <- withChunks (lineChunks (numCapabilities * 4))
         (mapReduce rnf (length . filter (isInfixOf "09:00:00") . LB.lines)
                    rnf sum)
         path
    print r
