module PodParser where

import PodTypes
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty(content)
import Data.Char
import Data.List

import Data.Maybe.Utils
import Data.Either.Utils

data Item = Item {itemtitle :: String,
                  enclosureurl :: String
                  }
          deriving (Eq, Show, Read)

data Feed = Feed {channeltitle :: String,
                  items :: [Item]}
            deriving (Eq, Show, Read)

{- | Given a podcast and an Item, produce an Episode -}
item2ep :: Podcast -> Item -> Episode
item2ep pc item =
    Episode {epId = 0,
             epCast = pc,
             epURL = enclosureurl item,
             epDone = False}

{- | Parse the data from a given string, with the given name to use
in error messages. -}
parse :: String -> String -> Feed
parse content name = 
    Feed {channeltitle = getTitle doc,
          items = getEnclosures doc}

    where parseResult = xmlParse name (stripUnicodeBOM content)
          doc = getContent parseResult

          getContent :: Document -> Content
          getContent (Document _ _ e _) = CElem e
          
          {- | Some Unicode documents begin with a binary sequence;
             strip it off before processing. -}
          stripUnicodeBOM :: String -> String
          stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
          stripUnicodeBOM x = x

unesc = xmlUnEscape stdXmlEscaper

{- | Pull out the channel part of the document.

Note that HaXml defines CFilter as:

> type CFilter = Content -> [Content]
-}
channel :: CFilter
channel = tag "rss" /> tag "channel"

getTitle :: Content -> String
getTitle doc =
    case channel /> tag "title" /> txt $ doc of
      [] -> "Untitled"          -- No title tag present
      (x:_) -> -- Found 1 (or more) title tags.  Take the first.
          show . content $ x 

getEnclosures :: Content -> [Item]
getEnclosures doc =
    concat . map procItem $ getItems doc
    where procItem :: Content -> [Item]
          procItem i = map (procEnclosure title) enclosure
              where title = case (keep /> tag "title" /> txt) i of
                              [] -> "Untitled"
                              (x:_) -> show . content $ x
                    enclosure = tag "enclosure" `o` children $ i

          procEnclosure :: String -> Content -> Item
          procEnclosure title e =
              Item {itemtitle = title,
                    enclosureurl = head0 $ forceMaybe $ stratt "url" e
                   }

          head0 :: [String] -> String
          head0 [] = ""
          head0 (x:xs) = x

          getItems :: CFilter
          getItems = channel /> tag "item"
       
              

--------------------------------------------------
-- Utilities
--------------------------------------------------

attrofelem :: String -> Content -> Maybe AttValue
attrofelem attrname (CElem inelem) =
    case unesc inelem of
      Elem name al _ -> lookup attrname al
attrofelem _ _ =
    error "attrofelem: called on something other than a CElem"
stratt :: String -> Content -> Maybe [String]
stratt attrname content =
    case attrofelem attrname content of
      Just (AttValue x) -> Just (concat . map mapfunc $ x)
      Nothing -> Nothing
    where mapfunc (Left x) = [x]
          mapfunc (Right _) = []

-- Finds the literal children of the named tag, and returns it/them
tagof :: String -> CFilter
tagof x = keep /> tag x -- /> txt

-- Retruns the literal string that tagof would find
strof :: String -> Content -> String
strof x y = forceEither $ strof_either x y

strof_either :: String -> Content -> Either String String
strof_either x y =
    case tagof x $ y of
      [CElem elem] -> Right $ verbatim $ tag x /> txt $ CElem (unesc elem)
      z -> Left $ "strof: expecting CElem in " ++ x ++ ", got "
           ++ verbatim z ++ " at " ++ verbatim y

strofm x y = 
    if length errors /= 0
       then Left errors
       else Right (concat plainlist)
    where mapped = map (strof_either x) $ y
          (errors, plainlist) = conveithers mapped
          isright (Left _) = False
          isright (Right _) = True

conveithers :: [Either a b] -> ([a], [b])
conveithers inp =  worker inp ([], [])
    where worker [] y = y
          worker (Left x:xs) (lefts, rights) =
              worker xs (x:lefts, rights)
          worker (Right x:xs) (lefts, rights) =
              worker xs (lefts, x:rights)

