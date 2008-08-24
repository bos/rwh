{-- snippet all --}
-- ch08/filter.hs

main = interact (unlines . filter (elem 'a') . lines)
{-- /snippet all --}

