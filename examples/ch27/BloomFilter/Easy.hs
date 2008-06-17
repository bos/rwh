{-- snippet easyList --}
module BloomFilter.Easy
    (
      suggestSizing
    , sizings
    , easyList
    ) where

import BloomFilter.Hash (Hashable, doubleHash)
import BloomFilter (Bloom, fromList)
import Data.Word (Word32)

easyList errRate values =
         fromList (doubleHash numHashes) numBits values
   where capacity = length values
         (numBits, numHashes) = suggestSizing capacity errRate
{-- /snippet easyList --}

{-- snippet suggestSizing --}
suggestSizing :: Int           -- expected maximum capacity
              -> Double        -- desired false positive rate
              -> (Word32, Int) -- (filter size, number of hashes)
suggestSizing capacity errRate
    | capacity <= 0                = error "invalid capacity"
    | errRate <= 0 || errRate >= 1 = error "invalid error rate"
    | otherwise                    = minimum (sizings capacity errRate)

sizings :: Int -> Double -> [(Word32, Int)]
sizings capacity errRate =
    [(round ((-k) * cap / log (1 - (errRate ** (1 / k)))), round k)
     | k <- [1..100]]
  where cap = fromIntegral capacity
{-- /snippet suggestSizing --}

{-- snippet easyList.type --}
easyList :: (Hashable a)
         => Double        -- false positive rate (between 0 and 1)
         -> [a]           -- values to populate the filter with
         -> Bloom a
{-- /snippet easyList.type --}
