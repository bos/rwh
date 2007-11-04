import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (chr, ord, isDigit)
import Data.Int (Int64)
import Data.Word (Word8)

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

{-- snippet then --}
(==>) :: Parse a -> (a -> Parse b) -> Parse b
x ==> f = Parse (\st -> case runParse x st of
                          Left err -> Left err
                          Right (a, st') -> runParse (f a) st')
{-- /snippet then --}

x ==>! f = x ==> \_ -> f

{-- snippet parse --}
parse :: Parse a -> L.ByteString -> Either String a
parse f s = case runParse f (ParseState s 0) of
              Left err -> Left err
              Right (a, _) -> Right a
{-- /snippet parse --}

instance Monad Parse where
    return = identity
    (>>=) = (==>)
    fail = bail

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

peekByte :: Parse (Maybe Word8)
peekByte = getState ==> (identity . fmap fst . uncons . string)

instance Functor Parse where
    fmap f p = Parse (\st -> case runParse p st of
                               Left err -> Left err
                               Right (a, st') -> let fa = identity (f a)
                                                 in runParse fa st')

parseEnd :: Parse Bool
parseEnd = (L.null . string) `fmap` getState

w2c = chr . fromIntegral

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c `fmap` peekByte
parseChar :: Parse Char
parseChar = w2c `fmap` parseByte

parseWhile :: (Char -> Bool) -> Parse [Char]
parseWhile p = peekChar ==> \mc ->
               case mc of
                 Nothing -> identity []
                 Just c | p c -> parseChar ==>!
                                 parseWhile p ==> \cs ->
                                 identity (c:cs)
                        | otherwise -> identity []

parseNat :: Parse Int
parseNat = parseWhile isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n
