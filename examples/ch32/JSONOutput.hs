module JSONOutput
    (
     jvalue
    ) where

import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ (Doc, Mode(..), TextDetails(..), (<>), (<+>), braces, brackets, char, comma, colon, double, doubleQuotes, fsep, fullRender, hcat, punctuate, render, text)
import Numeric (showHex)
import JSON (JSON(..), JValue(..), fromJObject, jobject)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Internal (c2w)
import qualified JSONBuilder as B

jvalue :: JValue -> Doc
jvalue (JString s) = string s
jvalue (JNumber n) = double (fromRational n)
jvalue (JObject o) = series braces field (fromJObject o)
jvalue (JArray a) = series brackets jvalue a
jvalue (JBool True) = text "true"
jvalue (JBool False) = text "false"
jvalue JNull = text "null"

unicode :: Char -> Doc
unicode c | d < 0x10000 = ch d
          | otherwise = astral (d - 0x10000)
    where d = fromEnum c
          ch x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
              where h = showHex x ""
          astral n = (a + 0xd800) <> ch (b + 0xdc00)
              where a = (n `shiftR` 10) .&. 0x3ff
                    b = n .&. 0x3ff

string :: String -> Doc
string = doubleQuotes . hcat . map one
    where one c = case M.lookup c specials of
                    Just r -> text r
                    Nothing | c < ' ' || c > '\xff' -> unicode c
                            | otherwise             -> char c
          specials = M.fromList [
            ('\b', "\\b"), ('\n', "\\n"), ('\f', "\\f"), ('\r', "\\r"),
            ('\t', "\\t"), ('\\', "\\\\"), ('\"', "\\\""), ('/', "\\/")]

series :: (Doc -> Doc) -> (a -> Doc) -> [a] -> Doc
series open item = open . fsep . punctuate comma . map item

field :: (String, JValue) -> Doc
field (k,v) = string k <> colon <+> jvalue v

renderS :: Doc -> C.ByteString
renderS = fullRender OneLineMode 0 0 item C.empty
    where item (Chr c) s = C.singleton c `C.append` s
          item (Str a) b = C.pack a `C.append` b
          item (PStr a) b = C.pack a `C.append` b

renderB :: Doc -> C.ByteString
renderB = B.toLazyByteString . fullRender OneLineMode 0 0 item B.empty
    where item (Chr c) s = B.singleton (c2w c) `B.append` s
          item (Str a) b = pack a `B.append` b
          item (PStr a) b = pack a `B.append` b
          pack = B.fromLazyByteString . C.pack
