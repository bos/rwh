{-- snippet all --}
-- ch07/filter.hs

main = interact (unlines . filter (elem 'a') . lines)
{-- /snippet all --}

