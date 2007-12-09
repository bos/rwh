{-- snippet Array --}
import Data.Array (Array(..), (!), bounds, elems, indices, ixmap, listArray)
{-- /snippet Array --}
import Data.Char (isDigit, ord)
import Data.Ratio (Ratio, (%))
import Data.Ix (Ix(..))
import Data.List (foldl', group, isInfixOf, mapAccumR, sort, sortBy, tails)
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

outerGuard = "101"
centerGuard = "01010"

listToArray xs = listArray (0,l-1) xs
    where l = length xs

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList
{-- /snippet encodingTables --}


encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map asDigit

asDigit :: Char -> Int
asDigit c | isDigit c = ord c - ord '0'
          | otherwise = error "not a digit"

encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where (left, right) = splitAt 5 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map (rightCodes !) (right ++ [checkDigit s])
        leftEncode '1' = (leftOddCodes !)
        leftEncode '0' = (leftEvenCodes !)

type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)

data Pixmap = Pixmap {
      pixWidth :: Int
    , pixHeight :: Int
    , pixMax :: Int
    , pixData :: Array (Int,Int) RGB
    } deriving (Eq)

instance Show Pixmap where
    show p = "Pixmap " ++ show (pixWidth p) ++ "x" ++ show (pixHeight p)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: Array (Int,Int) Pixel
    } deriving (Eq)

instance Show Greymap where
    show p = "Greymap " ++ show (greyWidth p) ++ "x" ++ show (greyHeight p)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 f = identity []
parseTimes n f = f ==> \x -> (x:) <$> parseTimes (n-1) f
    
parseRGB :: Parse (Pixel, Pixel, Pixel)
parseRGB = parseByte ==> \r ->
           parseByte ==> \g ->
           parseByte ==> \b ->
           identity (r,g,b)

parseRawPPM :: Parse Pixmap
parseRawPPM =
    parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
    assert (header == "P6") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxValue ->
    parseByte ==>&
    parseTimes (width * height) parseRGB ==> \pxs ->
    let pixmap = listArray ((0,0),(width-1,height-1)) pxs
    in identity (Pixmap width height maxValue pixmap)

luminance :: Pixmap -> Greymap
luminance p = Greymap {
                greyWidth = pixWidth p
              , greyHeight = pixHeight p
              , greyMax = pixMax p
              , greyData = lumPixel <$> pixData p
              }
    where lumPixel (r,g,b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
              where r' = fromIntegral r
                    g' = fromIntegral g
                    b' = fromIntegral b

row :: (Ix a, Ix b) => b -> Array (a,b) c -> Array a c
row j a = ixmap (l,u) project a
    where project i = (i,j)
          ((l,_), (u,_)) = bounds a

col :: (Ix a, Ix b) => a -> Array (a,b) c -> Array b c
col i a = ixmap (l,u) project a
    where project j = (i,j)
          ((_,l), (_,u)) = bounds a

foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
    where go s (j:js) = go (f s (a ! j)) js
          go s _ = s

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k a
threshold n a = binary <$> a
    where binary i | i < pivot = 0
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

(+&) :: Digit -> Digit -> Digit
a +& b = (a + b) `mod` 10

seqFirstDigit :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
seqFirstDigit key seq m =
    M.insert (key +& digit) (digit:renormalize qes) m
  where renormalize = mapEveryOther (`div` 3) . map fromParity
        digit = firstDigit qes
        qes = reverse seq

finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 [])
            . mapEveryOther (map (fmap (*3)))

-- | Generate a new solution map considering a new set of digits
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
updateMap digit key seq m = M.insert (key +& digit') (digit:seq) m
    where digit' = fromParity digit


(-*>) :: Either e a -> (a -> Either e b) -> Either e b
Left err -*> _ = Left err
Right a -*> f = f a

-- fnord :: L.ByteString -> Pixmap
parseGreymap :: L.ByteString -> Either String Greymap
parseGreymap bs = parse parseRawPPM bs -*> (Right . luminance)

maybeEither Nothing = Left "fail"
maybeEither (Just a) = Right a

withRow :: Int -> Greymap -> (RunLength Pixel -> t) -> t
withRow n greymap f = f . runLength . elems $ posterized
    where posterized = threshold 0.4 . row n . greyData $ greymap

fnord greymap =
    let center = greyHeight greymap `div` 2
    in withRow center greymap (fmap head . listToMaybe . match)
  where match = filter (not . null) . map (solve . candidateDigits) . tails

main = do
  args <- getArgs
  forM_ args $ \arg -> do
    e <- (parseGreymap) <$> L.readFile arg
    case e of
      Left err -> print $ "error: " ++ err
      Right greymap -> print (fnord greymap)

a :: [Int]
a=[1,1,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,1,0,0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,0,1,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
ra = runLength a
