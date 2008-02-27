module PodParser where

import PodTypes
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty(content)
import Text.XML.HaXml.Html.Generate(showattr)
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

contentToString :: Content -> String
contentToString = show . content

getTitle :: Content -> String
getTitle doc =
    case channel /> tag "title" /> txt $ doc of
      [] -> "Untitled"          -- No title tag present
      (x:_) -> -- Found 1 (or more) title tags.  Take the first.
          contentToString x

getEnclosures :: Content -> [Item]
getEnclosures doc =
    concatMap procItem $ getItems doc
    where procItem :: Content -> [Item]
          procItem i = concatMap (procEnclosure title) enclosure
              where title = case (keep /> tag "title" /> txt) i of
                              [] -> "Untitled"
                              (x:_) -> contentToString x
                    enclosure = (keep /> tag "enclosure") i

          procEnclosure :: String -> Content -> [Item]
          procEnclosure title enclosure =
              map makeItem (showattr "url" enclosure)
              where makeItem x = Item {itemtitle = title,
                                       enclosureurl = contentToString x}

          getItems :: CFilter
          getItems = channel /> tag "item"
