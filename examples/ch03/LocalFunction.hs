{-- snippet pluralise --}
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 1 = "1 " ++ word
          plural n = show n ++ " " ++ word ++ "s"
{-- /snippet pluralise --}
