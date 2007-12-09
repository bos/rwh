{-- snippet Array --}
import Data.Array (Array(..), (!), bounds, elems, indices, ixmap, listArray)
{-- /snippet Array --}
import Data.Char (isDigit, ord)
import Data.Ratio (Ratio, (%))
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Bits (shiftR, xor, (.&.))
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.Map as M

import Parse

{-- snippet checkDigit --}
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = zipWith (*) (cycle [3,1]) (reverse ds)
{-- /snippet checkDigit --}

{-- snippet encodingTables --}
leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList = map complement <$> leftOddList
    where complement '0' = '1'
          complement '1' = '0'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,l-1) xs
    where l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList
{-- /snippet encodingTables --}

{-- snippet encode --}
encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map asDigit

-- | This function computes the check digit; don't pass one in.
encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where (left, right) = splitAt 5 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

asDigit :: Char -> Int
asDigit c | isDigit c = ord c - ord '0'
          | otherwise = error "not a digit"

outerGuard = "101"
centerGuard = "01010"
{-- /snippet encode --}

{-- snippet Pixmap --}
type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)

type Pixmap = Array (Int,Int) RGB
{-- /snippet Pixmap --}

{-- snippet parseRawPPM --}
parseRawPPM :: Parse Pixmap
parseRawPPM =
    parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
    assert (header == "P6") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxValue ->
    assert (maxValue == 255) "max value out of spec" ==>&
    parseByte ==>&
    parseTimes (width * height) parseRGB ==> \pxs ->
    identity (listArray ((0,0),(width-1,height-1)) pxs)

parseRGB :: Parse RGB
parseRGB = parseByte ==> \r ->
           parseByte ==> \g ->
           parseByte ==> \b ->
           identity (r,g,b)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> (x:) <$> parseTimes (n-1) p
{-- /snippet parseRawPPM --}
    
{-- snippet luminance --}
luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r,g,b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
    where r' = fromIntegral r
          g' = fromIntegral g
          b' = fromIntegral b
{-- /snippet luminance --}

{-- snippet pixmapToGreymap --}
type Greymap = Array (Int,Int) Pixel

pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance
{-- /snippet pixmapToGreymap --}

row :: (Ix a, Ix b) => b -> Array (a,b) c -> Array a c
row j a = ixmap (l,u) project a
    where project i = (i,j)
          ((l,_), (u,_)) = bounds a

{-- snippet fold --}
-- | Strict left fold, similar to foldl' on lists.
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
    where go s (j:js) = let s' = f s (a ! j)
                        in s' `seq` go s' js
          go s _ = s

-- | Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists.
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a
{-- /snippet fold --}

data Bit = On | Off
           deriving (Eq, Ord, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k a
threshold n a = binary <$> a
    where binary i | i < pivot  = 0
                   | otherwise  = 1
          pivot = round $ least + (greatest - least) * n
          least = fromIntegral $ choose (<) a
          greatest = fromIntegral $ choose (>) a
          choose f = foldA1 $ \a b -> if f a b then a else b

type Run = Int
type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
    where rle xs = (length xs, head xs)

runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength

type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map (% sum xs) xs

asSRL :: [String] -> [[Score]]
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList

chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h, t) = f xs
                 in h : chunkWith f t

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)

bestScores :: [[Score]] -> [Run] -> [(Score, Digit)]
bestScores srl ps = take 3 . sort $ scores
    where scores = zip [score d (scaleToOne ps) | d <- srl] digits
          score a b = sum . map abs $ zipWith (-) a b
          digits = [0..9]

firstDigit :: [Parity a] -> Digit
firstDigit = snd
           . head
           . bestScores paritySRL
           . runLengths
           . map parityBit
           . take 6
  where parityBit (Even _) = 0
        parityBit (Odd _) = 1

data Parity a = Even a | Odd a | None a
                deriving (Show)

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
    fmap = parityMap

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

type Digit = Word8

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on f g x y = g x `f` g y

instance Eq a => Eq (Parity a) where
    (==) = (==) `on` fromParity

instance Ord a => Ord (Parity a) where
    compare = compare `on` fromParity

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sort ((map Odd (bestScores leftOddSRL ps)) ++
                    (map Even (bestScores leftEvenSRL ps)))

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL

-- | For each digit position in a scan, generate a sorted list of
-- possible matches at that position, along with the parity with which
-- which every match was encoded.
candidateDigits :: RunLength Pixel -> [[Parity Digit]]
candidateDigits ((_, 1):_) = []
candidateDigits rle =
    if all (not . null) match
    then map (map (fmap snd)) match
    else []
  where match = map bestLeft left ++ map bestRight right
        left = chunksOf 4 . take 24 . drop 3 $ runLengths
        right = chunksOf 4 . take 24 . drop 32 $ runLengths
        (runLengths, bits) = unzip rle

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith id (cycle [f,id])

type DigitMap = M.Map Digit [Digit]
type ParityMap = M.Map Digit [Parity Digit]

solve :: [[Parity Digit]] -> [[Digit]]
solve [] = []
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
    where checkDigits = map fromParity (last xs)
          m = buildMap (init xs)
          addCheckDigit m k = (++[k]) <$> M.lookup k m

buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (10 -)
         . addFirstDigit
         . finalDigits

addFirstDigit :: ParityMap -> DigitMap
addFirstDigit = M.foldWithKey seqFirstDigit M.empty

insertMap :: Digit -> Digit -> [a] -> M.Map Digit [a] -> M.Map Digit [a]
insertMap key digit val m = val `seq` M.insert key' val m
    where key' = (key + digit) `mod` 10

seqFirstDigit :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
seqFirstDigit key seq = insertMap key digit (digit:renormalize qes)
  where renormalize = mapEveryOther (`div` 3) . map fromParity
        digit = firstDigit qes
        qes = reverse seq

finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 [])
            . mapEveryOther (map (fmap (*3)))

-- | Generate a new solution map, considering a new set of digits.
incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
incorporateDigits old digits = foldl' (useDigit old) M.empty digits

-- | Update a new solution map based on a single digit and an old
-- solution map.
useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit =
  new `M.union` M.foldWithKey (updateMap digit) M.empty old

-- | Create a new map entry from the previous data.
updateMap :: Parity Digit -> Digit -> [Parity Digit] -> ParityMap
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)


parseGreymap :: L.ByteString -> Either String Greymap
parseGreymap bs = case parse parseRawPPM bs of
                    Left err -> Left err
                    Right a -> Right (pixmapToGreymap a)

withRow :: Int -> Greymap -> (RunLength Pixel -> t) -> t
withRow n greymap f = f . runLength . elems $ posterized
    where posterized = threshold 0.4 . row n $ greymap

findEAN13 :: Greymap -> Maybe [Digit]
findEAN13 greymap =
    withRow center greymap (fmap head . listToMaybe . match)
  where match = filter (not . null) . map (solve . candidateDigits) . tails
        (_, (maxX, maxY)) = bounds greymap
        center = (maxX + 1) `div` 2

main = do
  args <- getArgs
  forM_ args $ \arg -> do
    e <- (parseGreymap) <$> L.readFile arg
    case e of
      Left err -> print $ "error: " ++ err
      Right greymap -> print $ findEAN13 greymap

a :: [Int]
a=[1,1,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,1,0,0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,0,1,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
ra = runLength a
