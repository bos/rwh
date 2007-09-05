{-- snippet all --}
import Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile = endBy line eol

-- Each line contains 1 or more cells, separated by a comma
line = sepBy cell (char ',')

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cell = many (noneOf ",\n")

-- The end of line character is \n
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
{-- /snippet all --}
