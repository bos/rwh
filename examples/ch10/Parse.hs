module Parse
    (
      ParseState(..)
    , Parse(..)
    , (==>)
    , (==>&)
    , parseByte
    , parseNat
    , skipSpaces
    , assert
    , w2c
    , parseWhile
    , parseWhileWith
    , identity
    , parse
    ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (chr, ord, isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)
import Debug.Trace

import PNM (Greymap(..))

{-- snippet ParseState --}
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)
{-- /snippet ParseState --}

{-- snippet Parse --}
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
{-- /snippet Parse --}

{-- snippet identity --}
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
{-- /snippet identity --}

{-- snippet bail --}
bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err
{-- /snippet bail --}

{-- snippet bind --}
(==>) :: Parse a -> (a -> Parse b) -> Parse b
x ==> f = Parse (\st -> case runParse x st of
                          Left err -> Left err
                          Right (a, st') -> runParse (f a) st')
{-- /snippet bind --}

{-- snippet parse --}
parse :: Parse a -> L.ByteString -> Either String a
parse f s = case runParse f (ParseState s 0) of
              Left err -> Left err
              Right (a, _) -> Right a
{-- /snippet parse --}

{-- snippet Monad --}
instance Monad Parse where
    return = identity
    (>>=) = (==>)
    fail = bail
{-- /snippet Monad --}

{-- snippet getPut --}
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))
{-- /snippet getPut --}

{-- snippet uncons --}
uncons :: L.ByteString -> Maybe (Word8, L.ByteString)
uncons s = if L.null s
           then Nothing
           else Just (L.head s, L.tail s)
{-- /snippet uncons --}

{-- snippet parseByte --}
parseByte :: Parse Word8
parseByte =
    getState ==> \st ->
    case uncons (string st) of
      Nothing -> bail "no more input"
      Just (c, s) -> let st' = st { string = s, offset = offset st + 1 }
                     in putState st' ==> \_ -> identity c
{-- /snippet parseByte --}

{-- snippet peekByte --}
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . uncons . string) <$> getState
{-- /snippet peekByte --}

{-- snippet Functor --}
instance Functor Parse where
    fmap f p = Parse (\st -> case runParse p st of
                               Left err -> Left err
                               Right (a, st') -> let fa = identity (f a)
                                                 in runParse fa st')
{-- /snippet Functor --}

{-- snippet peekChar --}
peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte
{-- /snippet peekChar --}

{-- snippet parseChar --}
w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte
{-- /snippet parseChar --}

{-- snippet parseWhile --}
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []
{-- /snippet parseWhile --}

{-- snippet parseWhileVerbose --}
parseWhileVerbose p =
    peekByte ==> \mc ->
    case mc of
      Nothing -> identity []
      Just _ -> parseByte ==> \b ->
                if p b
                then parseWhileVerbose p ==> \bs ->
                     identity (b:bs)
                else identity []
{-- /snippet parseWhileVerbose --}

{-- snippet parseNat --}
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n
{-- /snippet parseNat --}

{-- snippet helpers --}
(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err
{-- /snippet helpers --}

{-- snippet parseBytes --}
parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h
{-- /snippet parseBytes --}

{-- snippet parseRawPGM --}
parseRawPGM =
    parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
{-- /snippet parseRawPGM --}
