{-- snippet all --}
-- ch06/toupper-lazy5.hs

import Data.Char(toUpper)

main = interact (map toUpper . (++) "Your data, in uppercase, is:\n\n")
{-- /snippet all --}

