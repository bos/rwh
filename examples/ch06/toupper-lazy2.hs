{-- snippet all --}
-- ch06/toupper-lazy1.hs

import System.IO
import Data.Char(toUpper)

main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       hPutStr outh (map toUpper inpstr)
       hClose inh
       hClose outh
{-- /snippet all --}

