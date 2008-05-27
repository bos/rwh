{-- snippet minimum --}
minimum    :: (Ord a) => [a] -> a
minimum [] =  error "Prelude.minimum: empty list"
minimum xs =  foldl1 min xs
{-- /snippet minimum --}
