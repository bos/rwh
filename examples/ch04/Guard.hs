{-- snippet secondEqualsThird --}
secondEqualsThird x = case x of
                        (True, a, b) | a == b    -> "foo"
                                     | otherwise -> "bar"
{-- /snippet secondEqualsThird -}
