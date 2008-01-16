{-- snippet add --}
add a b = a + b
{-- /snippet add --}

{-- snippet sumList --}
sumList (x:xs) = x + sumList xs
sumList []     = 0
{-- /snippet sumList --}
