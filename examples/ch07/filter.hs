{-- snippet all --}
-- ch06/filter.hs

main = interact (unlines . filter (elem 'a') . lines)
{-- /snippet all --}

