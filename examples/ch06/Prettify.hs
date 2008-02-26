module Prettify
    (
    -- * Constructors
      Doc
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
         | Line Bool
         | Concat Doc Doc
         | Union Doc Doc
{-- /snippet Doc --}

instance Monoid Doc where
    mempty = empty
    mappend = (<>)

{-- snippet append --}
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y
{-- /snippet append --}

{-- snippet basic --}
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)
{-- /snippet basic --}

{-- snippet line --}
line :: Doc
line = Line False
{-- /snippet line --}

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = Union (flatten x) x
    where flatten (Concat x y) = Concat (flatten x) (flatten y)
          flatten (Line break) = if break then Empty else Text " "
          flatten (Union x y)  = flatten x
          flatten other        = other

{-- snippet punctuate --}
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
punctuate p _      = []
{-- /snippet punctuate --}

compact :: Int -> Doc -> String
compact _ x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty -> transform ds
                Char c -> c : transform ds
                Text s -> s ++ transform ds
                Line _ -> transform ds
                Concat a b -> transform (a:b:ds)
                Union _ b -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty w x = best 0 [x]
    where best n (d:ds) =
              case d of
                Empty -> best n ds
                Char c -> c :  best (n + 1) ds
                Text s -> s ++ best (n + length s) ds
                Line _ -> '\n' : best n ds
                Concat a b -> best n (a:b:ds)
                Union a b -> nicest n (best n (a:ds))
                                      (best n (b:ds))
          best _ _ = ""
          nicest n a b | min w n `fits` a = a
                       | otherwise = b
          w `fits` x | w < 0 = False
          w `fits` "" = True
          w `fits` ('\n':_) = True
          w `fits` (c:cs) = (w - 1) `fits` cs

instance Show Doc where
    show doc = pretty 80 doc
