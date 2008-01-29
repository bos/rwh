module JSONBuilder where

import Foreign (ForeignPtr, withForeignPtr, Ptr, plusPtr, sizeOf, poke)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString as S
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

newtype JSONBuilder = JB {
      runJB :: (Buffer -> [S.ByteString]) -> Buffer -> [S.ByteString]
    }

data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

append :: JSONBuilder -> JSONBuilder -> JSONBuilder
append (JB f) (JB g) = JB (f . g)

toLazyByteString :: JSONBuilder -> L.ByteString
toLazyByteString m = L.fromChunks $ unsafePerformIO $ do
    buf <- newBuffer defaultSize
    return (runJB (m `append` flush) (const []) buf)

flush :: JSONBuilder
flush = JB $ \ k buf@(Buffer p o u l) ->
    if u == 0
      then k buf
      else S.PS p o u : k (Buffer p (o+u) 0 l)

newBuffer :: Int -> IO Buffer
newBuffer size = do
    fp <- S.mallocByteString size
    return $! Buffer fp 0 0 size

defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

singleton :: Word8 -> JSONBuilder
singleton = writeN 1 . flip poke

empty :: JSONBuilder
empty = JB id

ensureFree :: Int -> JSONBuilder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l then empty else
        flush `append` unsafeLiftIO (const (newBuffer (max n defaultSize)))

writeN :: Int -> (Ptr Word8 -> IO ()) -> JSONBuilder
writeN n f = ensureFree n `append` unsafeLiftIO (writeNBuffer n f)

writeNBuffer :: Int -> (Ptr Word8 -> IO ()) -> Buffer -> IO Buffer
writeNBuffer n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))

unsafeLiftIO :: (Buffer -> IO Buffer) -> JSONBuilder
unsafeLiftIO f =  JB $ \ k buf -> unsafePerformIO $ do
    buf' <- f buf
    return (k buf')

withSize :: (Int -> JSONBuilder) -> JSONBuilder
withSize f = JB $ \ k buf@(Buffer _ _ _ l) ->
    runJB (f l) k buf

fromLazyByteString :: L.ByteString -> JSONBuilder
fromLazyByteString bss = flush `append` mapBuilder (L.toChunks bss ++)

mapBuilder :: ([S.ByteString] -> [S.ByteString]) -> JSONBuilder
mapBuilder f = JB (f .)
