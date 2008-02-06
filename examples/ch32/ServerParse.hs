module ServerParse
    (
      HttpRequest(..)
    , Method(..)
    , p_query
    , p_request
    , parse
    ) where

import ApplicativeParsec
import Numeric (readHex)
import Control.Monad (liftM4)
import System.IO (Handle)

urlBaseChars :: [Char]
urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_query :: CharParser () [(String, Maybe String)]
p_query = pair `sepBy` char '&'
  where pair = liftA2 (,) (many1 safe <?> "parameter name")
                          (optional (char '=' *> many safe))
        safe = oneOf urlBaseChars
           <|> char '%' *> liftA2 diddle hexDigit hexDigit
           <|> ' ' <$ char '+'
           <?> "safe"
        diddle a b = toEnum . fst . head . readHex $ [a,b]

crlf :: CharParser st ()
crlf = (() <$ string "\r\n" <?> "cr-lf") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

p_headers :: CharParser st [(String, String)]
p_headers = manyTill header crlf
  where header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
        fieldName = liftA2 (:) letter (many fieldChar)
        fieldChar = letter <|> digit <|> oneOf "-_"
        contents = liftA2 (++) (many1 notEOL <* crlf)
                               (continuation <|> pure [])
        continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents

data Method = Get | Post
          deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest {
      reqMethod :: Method
    , reqURL :: String
    , reqHeaders :: [(String, String)]
    , reqBody :: Maybe String
    , reqHandle :: Handle
    } deriving (Eq, Show)

p_request :: CharParser () (Handle -> HttpRequest)
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> many anyChar)
  where q s c p = liftM4 HttpRequest (c <$ string s <* char ' ') url p_headers p
        url = manyTill notEOL (try $ string " HTTP/1." <* oneOf "01") <* crlf
