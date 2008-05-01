#!/usr/bin/env runghc

-- | This program prints the HTML index for a particular edition
-- (alpha, beta, or complete) of the book.

import Control.Monad (unless)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ

type Date = String

data PubStatus = Alpha Date
               | Beta Date
               | Unpublished
                 deriving (Eq, Show)
               
date (Alpha d) = d
date (Beta d) = d
date _ = "Unpublished"

data Kind = Preface | Chapter | Appendix
          deriving (Eq, Ord, Show)

data Entry = Entry {
      kind :: Kind
    , baseName :: FilePath
    , title :: String
    , status :: PubStatus
    } deriving (Eq, Show)

pr = Entry Preface
ch = Entry Chapter
app = Entry Appendix

chapters = [
   pr "preface" "Preface" Unpublished
 , ch "whyfp" "Why functional programming? Why Haskell?" Unpublished
 , ch "starting" "Getting started" $ Beta "2008-01-21"
 , ch "funcstypes" "Types and functions" $ Beta "2008-01-21"
 , ch "deftypes" "Defining types, streamlining functions" $ Beta "2008-01-21"
 , ch "fp" "Functional programming" $ Alpha "2008-04-22"
 , ch "library" "Writing a library" $ Alpha "2008-04-22"
 , ch "typeclasses" "Using typeclasses" $ Beta "2008-02-21"
 , ch "io" "Input and output" $ Beta "2008-02-21"
 , ch "glob" "Case study: regular expressions and file name matching"
   $ Beta "2008-01-21"
 , ch "find" "I/O case study: a library for searching the filesystem"
   $ Beta "2008-04-22"
 , ch "binary" "Code case study: parsing a binary data format"
   $ Beta "2008-04-22"
 , ch "testing" "Testing your code" Unpublished
 , ch "barcode" "Barcode recognition" $ Alpha "2008-04-22"
 , ch "data" "Data structures" $ Beta "2008-04-22"
 , ch "monads" "Monads" $ Beta "2008-04-22"
 , ch "monadcase" "Programming with monads" $ Beta "2008-04-30"
 , ch "monadtrans" "Monad transformers" $ Beta "2008-04-30"
 , ch "parsec" "The Parsec parsing library" $ Alpha "2008-04-22"
 , ch "ffi" "The foreign function interface" Unpublished
 , ch "errors" "Error handling" Unpublished
 , ch "systems" "Systems programming" $ Alpha "2008-04-22"
 , ch "databases" "Working with databases" $ Beta "2008-04-22"
 , ch "webclient" "Web client programming" $ Alpha "2008-04-22"
 , ch "gui" "GUI programming" $ Alpha "2008-04-22"
 , ch "concurrent" "Basic concurrent and parallel programming" Unpublished
 , ch "profiling" "Profiling and tuning for performance" Unpublished
 , ch "advhs" "Advanced language features" Unpublished
 , ch "sockets" "Network programming" $ Alpha "2008-04-22"
 , ch "webapp" "A concurrent RESTful web application" Unpublished
 , app "install" "Installing GHC" $ Beta "2008-01-21"
 , app "escapes" "Characters, strings, and escaping rules" $ Beta "2008-01-21"
 , app "web" "Web site and comment system usage and policies" $
   Beta "2008-01-21"
 ]

seqId :: Kind -> Int -> Doc
seqId Chapter n = int n <> text ". "
seqId Appendix n = (char . toEnum $ fromEnum 'A' + n) <> text ". "
seqId _ _ = empty

(&=) :: String -> String -> Doc
a &= b = space <> text a <> equals <> doubleQuotes (text b)
infix 4 &=

inside :: String -> Doc -> Doc -> Doc
inside tag kvs doc
    | isEmpty doc = char '<' <> text tag <> kvs <> text "/>"
    | otherwise = char '<' <> text tag <> kvs <> char '>' <>
                  doc <> text "</" <> text tag <> char '>'

stripes = cycle ["zebra_a", "zebra_b"]

alpha (Alpha _) = True
alpha (Beta _) = True
alpha _ = False

beta (Beta _) = True
beta _ = False

toc p = renderStyle style{mode=LeftMode} $
        header $+$
        (inside "div" ("class" &= "book") $
          inside "ul" ("class" &= "booktoc") $
            entry p stripes Preface chapters 0) $+$
        footer

header :: Doc
header =
    text "<!-- -*- html -*- -->" $+$
    text "{% extends \"boilerplate.html\" %}" $+$
    text "{% block bodycontent %}" $+$
    (inside "div" ("class" &= "navheader") $
      inside "h1" ("class" &= "booktitle") $
        (text "Real World Haskell " <>
         inside "span" ("class" &= "beta") (text "(beta)") <>
         inside "div" ("class" &= "authors") 
           (text "by Bryan O\'Sullivan, Don Stewart, and John Goerzen")))

footer :: Doc
footer = text "{% endblock %}"

entry :: (PubStatus -> Bool) -> [String] -> Kind -> [Entry] -> Int -> Doc
entry p zs@(z:zs') oldKind es@(e@(Entry kind _ _ _):es') n
      | oldKind /= kind = entry p zs kind es 1
      | otherwise = single p z e n $+$ entry p zs' kind es' (n+1)
entry _ _ _ _ _ = empty

single :: (PubStatus -> Bool) -> String -> Entry -> Int -> Doc
single p z e@(Entry kind name title when) n
  | p when =
    inside "li" ("class" &= z) $
     (inside "span" ("class" &= "chapinfo") $
      text (date when) <>
      (inside "a" ("href" &= "/feeds/comments/" ++ name ++ "/") $
       inside "img" ("src" &= "/support/figs/rss.png") empty)) <>
        seqId kind n <> inside "a" ("href" &= name ++ ".html") (text title)
  | otherwise =
    inside "li" ("class" &= z) $
     inside "span" ("class" &= "unpublished") $
     seqId kind n <> text title

main = do
  writeFile "index.alpha.html.in" (toc alpha)
  writeFile "index.beta.html.in" (toc beta)
  writeFile "index.complete.html.in" . toc $ const True
  writeFile ".stamp-indices" ""
  let instruct desc p = do
         let cs = map ((++".html") . baseName) . filter (not . p . status) $ chapters
         unless (null cs) $ do
           putStrLn $ "To delete non-" ++ desc ++ " chapters:"
           putStrLn $ "rm -f " ++ concat (intersperse " " cs)
  instruct "alpha" alpha
  instruct "beta" beta
