{-- snippet all --}
-- ch06/toupper-lazy3.hs

import Data.Char(toUpper)

main = do 
       inpStr <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inpStr)
{-- /snippet all --}

