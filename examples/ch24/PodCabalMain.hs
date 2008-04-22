{-- snippet all --}
-- ch24/PodCabalMain.hs
module Main where

import qualified PodMainGUI
import Paths_Pod(getDataFileName)

main = 
    do gladefn <- getDataFileName "podresources.glade"
       PodMainGUI.main gladefn
{-- /snippet all --}