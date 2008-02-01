module JSONParsec
    (
      p_text
    ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (*>), (<*))
import Control.Monad (ap, mzero)
import Data.Char (isHexDigit)
import Data.List (foldl')
import JSON (JObject, JValue(..), jobject)
import Numeric (readFloat, readHex, readSigned)
import Text.ParserCombinators.Parsec

instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

p_text :: GenParser Char () JValue
p_text = spaces *> text
         <?> "JSON text"
    where text = JObject <$> p_object
             <|> JArray <$> p_series '[' p_value ']'

p_value :: CharParser () JValue
p_value = value <* spaces
  where value = JString <$> p_string
            <|> JNumber <$> p_number
            <|> JObject <$> p_object
            <|> JArray <$> p_series '[' p_value ']'
            <|> JBool <$> (True <$ string "true" <|> False <$ string "false")
            <|> JNull <$ string "null"
            <?> "JSON value"

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> special <|> satisfy (`notElem` "\"\\")
          ch c = c <$ char c
          special = foldl1 (<|>) (map ch "\b\n\f\r\t\\\"/") <|> unicode
                <?> "escape character"
          unicode = char 'u' *> count 4 (satisfy isHexDigit) >>= check
          check x | code <= maxChar = pure (toEnum code)
                  | otherwise       = mzero
              where ((code,_):_) = readHex x
                    maxChar = fromEnum (maxBound :: Char)

p_number :: CharParser () Rational
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> mzero

p_series :: Char -> GenParser Char () a -> Char -> GenParser Char () [a]
p_series l p r = between (char l) (char r) $
                 (p <* spaces) `sepBy` char ',' <* spaces

p_object :: GenParser Char () (JObject JValue)
p_object = jobject <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value
