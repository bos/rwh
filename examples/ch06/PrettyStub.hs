module PrettyStub where

import Prettify (Doc(..))

{-- snippet stubs --}
text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
{-- /snippet stubs --}

{-- snippet append --}
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined
{-- /snippet append --}

{-- snippet hcat --}
hcat :: [Doc] -> Doc
hcat xs = undefined
{-- /snippet hcat --}

{-- snippet fsep --}
fsep :: [Doc] -> Doc
fsep xs = undefined
{-- /snippet fsep --}
