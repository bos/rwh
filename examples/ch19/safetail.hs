{-- snippet all --}
-- ch20/safetail.hs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
{-- /snippet all --}
