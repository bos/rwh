module JSONOutput
    (
      jvalue
    ) where

import qualified Data.Map as M
import Prettify (Doc, (<>), (<+>), char, double, enclose, encloseC, fsep, hcat, punctuate, text)
import Numeric (showHex)
import JSON (JValue(..), fromJArray, fromJObject)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Internal (c2w)
import qualified JSONBuilder as B

import JSON
import Prettify

jvalue :: JValue -> Doc
jvalue (JString s) = string s
jvalue (JNumber n) = double (fromRational n)
jvalue (JObject o) = series (encloseC '{' '}') field (fromJObject o)
jvalue (JArray a) = series (encloseC '(' ')') jvalue (fromJArray a)
jvalue (JBool True) = text "true"
jvalue (JBool False) = text "false"
jvalue JNull = text "null"

unicode :: Char -> Doc
unicode c | d < 0x10000 = ch d
          | otherwise = astral (d - 0x10000)
    where d = fromEnum c
          ch x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
              where h = showHex x ""
          astral n = ch (a + 0xd800) <> ch (b + 0xdc00)
              where a = (n `shiftR` 10) .&. 0x3ff
                    b = n .&. 0x3ff

string :: String -> Doc
string = encloseC '\"' '\"' . hcat . map one
    where one c = case M.lookup c specials of
                    Just r -> text r
                    Nothing | c < ' ' || c > '\xff' -> unicode c
                            | otherwise             -> char c
          specials = M.fromList [
            ('\b', "\\b"), ('\n', "\\n"), ('\f', "\\f"), ('\r', "\\r"),
            ('\t', "\\t"), ('\\', "\\\\"), ('\"', "\\\""), ('/', "\\/")]

series :: (Doc -> Doc) -> (a -> Doc) -> [a] -> Doc
series open item = open . fsep . punctuate (char ',') . map item

field :: (String, JValue) -> Doc
field (k,v) = string k <> char ':' <+> jvalue v
