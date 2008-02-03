module JSONParsec
    (
      p_text
    , parseFromFile
    ) where

import Data.Char (isHexDigit)
import JSON (JArray, JObject, JValue(..), jarray, jobject)
import Numeric (readFloat, readHex, readSigned)
import ApplicativeParsec

p_text :: GenParser Char () JValue
p_text = spaces *> text
         <?> "JSON text"
    where text = JObject <$> p_object
             <|> JArray <$> p_array

p_value :: CharParser () JValue
p_value = value <* spaces
  where value = JString <$> p_string
            <|> JNumber <$> p_number
            <|> JObject <$> p_object
            <|> JArray <$> p_array
            <|> JBool <$> (True <$ string "true" <|> False <$ string "false")
            <|> JNull <$ string "null"
            <?> "JSON value"

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> special <|> satisfy (`notElem` "\"\\")
          special = foldl1 (<|>) escapes <|> unicode
                <?> "escape character"
          escapes = zipWith ch "bnfrt\\\"/" "\b\n\f\r\t\\\"/"
          ch c r = r <$ char c
          unicode = char 'u' *> count 4 (satisfy isHexDigit) >>= check
          check x | code <= maxChar = pure (toEnum code)
                  | otherwise       = empty
              where ((code,_):_) = readHex x
                    maxChar = fromEnum (maxBound :: Char)

p_number :: CharParser () Rational
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

p_series :: Char -> GenParser Char () a -> Char -> GenParser Char () [a]
p_series l p r = between (char l <* spaces) (char r) $
                 (p <* spaces) `sepBy` (char ',' <* spaces)

p_object :: GenParser Char () (JObject JValue)
p_object = jobject <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value

p_array :: GenParser Char () (JArray JValue)
p_array = jarray <$> p_series  '[' p_value ']'
