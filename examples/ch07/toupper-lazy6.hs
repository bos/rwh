{-- snippet all --}
-- ch08/toupper-lazy6.hs

import Data.Char(toUpper)

main = interact ((++) "Your data, in uppercase, is:\n\n" . 
                 map toUpper)

{-- /snippet all --}

