{-- snippet all --}
-- ch06/toupper-lazy1.hs

import System.IO
import Data.Char(toUpper)

main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       let result = map toUpper inpStr
       hPutStr outh result
{-- /snippet all --}

