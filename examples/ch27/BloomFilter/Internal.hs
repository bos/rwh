{-- snippet Bloom --}
module BloomFilter.Internal
    (
      Bloom(..)
    , MutBloom(..)
    ) where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

data Bloom a = B {
      blmHash  :: (a -> [Word32])
    , blmArray :: UArray Word32 Bool
    }
{-- /snippet Bloom --}

{-- snippet MutBloom --}
data MutBloom s a = MB {
      mutHash :: (a -> [Word32])
    , mutArray :: STUArray s Word32 Bool
    }
{-- /snippet MutBloom --}

