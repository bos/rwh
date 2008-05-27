module Prettify2
    (
    -- * Constructors
      Doc(..)
    -- * Basic combinators
    , (<>)
    , empty
    , char
    , text
    , line
    -- * Derived combinators
    , double
    , fsep
    , hcat
    , punctuate
    -- * Renderers
    , compact
    , pretty
    ) where

import Data.Monoid (Monoid(..))

{-- snippet Doc --}

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)

{-- /snippet Doc --}

instance Monoid Doc where
    mempty  = empty
    mappend = (<>)

{-- snippet append --}
empty :: Doc
(<>)  :: Doc -> Doc -> Doc
{-- /snippet append --}

empty = Empty

Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

{-- snippet line --}
line :: Doc
line = Line
{-- /snippet line --}

{-- snippet hcat --}
hcat :: [Doc] -> Doc
hcat = fold (<>)
{-- /snippet hcat --}

{-- snippet fold --}
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty
{-- /snippet fold --}

{-- snippet fsep --}
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line
{-- /snippet fsep --}

{-- snippet group --}
group :: Doc -> Doc
group x = flatten x `Union` x
{-- /snippet group --}

{-- snippet flatten --}
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Text " "
-- flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other
{-- /snippet flatten --}

{-- snippet punctuate --}
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
punctuate p _      = []
{-- /snippet punctuate --}

{-- snippet compact --}
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)
{-- /snippet compact --}


{-- snippet pretty --}
-- pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""
{-- /snippet pretty --}

{-- snippet nicest --}
          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col
{-- /snippet nicest --}

{-- snippet fits --}
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs
{-- /snippet fits --}

{-- snippet nest --}
nest :: Int -> Doc -> Doc
{-- /snippet nest --}
nest = undefined

{-- snippet fill --}
fill :: Int -> Doc -> Doc
{-- /snippet fill --}
fill = undefined

--instance Show Doc where
--    show doc = pretty 80 doc
