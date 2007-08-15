{-- snippet splitLines --}
splitLines :: String -> [String]

splitLines [] = []
splitLines cs = case break isLineSeparator cs of 
                  (pre, '\r':'\n':suf) -> pre : splitLines suf
                  (pre, '\r':suf) -> pre : splitLines suf
                  (pre, '\n':suf) -> pre : splitLines suf
                  (pre, "") -> [pre]

isLineSeparator :: Char -> Bool
isLineSeparator c = c `elem` "\r\n"
{-- /snippet splitLines --}
