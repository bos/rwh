{-- snippet BloomCheck --}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import BloomFilter.Hash (Hashable)
import Data.Word (Word8, Word32)
import System.Random (Random(..), RandomGen)
import Test.QuickCheck
import qualified BloomFilter.Easy as B
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
{-- /snippet BloomCheck --}

{-- snippet handyCheck --}
handyCheck :: Testable a => Int -> a -> IO ()
handyCheck limit = check defaultConfig {
                     configMaxTest = limit
                   , configEvery   = \_ _ -> ""
                   }
{-- /snippet handyCheck --}

{-- snippet Word8 --}
instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)

instance Arbitrary Word8 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary
{-- /snippet Word8 --}

{-- snippet Word32 --}
integralCoarbitrary n =
    variant $ if m >= 0 then 2*m else 2*(-m) + 1
  where m = fromIntegral n

integralRandomR (a,b) g = case randomR (c,d) g of
                            (x,h) -> (fromIntegral x, h)
    where (c,d) = (fromIntegral a :: Integer,
                   fromIntegral b :: Integer)

instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)

instance Arbitrary Word32 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary
{-- /snippet Word32 --}

{-- snippet ByteString --}
instance Arbitrary Lazy.ByteString where
    arbitrary = Lazy.pack `fmap` arbitrary
    coarbitrary = coarbitrary . Lazy.unpack

instance Arbitrary Strict.ByteString where
    arbitrary = Strict.pack `fmap` arbitrary
    coarbitrary = coarbitrary . Strict.unpack
{-- /snippet ByteString --}

{-- snippet prop_one_present --}
(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k

prop_one_present :: (Hashable a) => a -> (a, Double) -> Bool
prop_one_present _ (elt,errRate) =
    B.easyList errRate [elt] =~> \filt ->
    elt `B.elem` filt
{-- /snippet prop_one_present --}

{-- snippet prop_all_present --}
prop_all_present :: (Hashable a) => a -> ([a], Double) -> Bool
prop_all_present _ (xs,errRate) = B.easyList errRate xs =~> \filt ->
                                  all (`B.elem` filt) xs
{-- /snippet prop_all_present --}

prop_suggestions_sane errRate =
    forAll (choose (1,fromIntegral maxWord32 `div` 8)) $ \cap ->
    (fst . minimum $ B.sizings cap errRate) < fromIntegral maxWord32 ==>
    either (const False) sane $ B.suggestSizing cap errRate
  where sane (bits,hashes) = bits > 0 && bits < maxBound && hashes > 0
        maxWord32 = maxBound :: Word32
