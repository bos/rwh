module PodParser where

import PodTypes
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Data.Char
import Data.List

data Item = Item {itemtitle :: String,
                  enclosureurl :: String
                  }
          deriving (Eq, Show, Read)

data Feed = Feed {channeltitle :: String,
                  items :: [Item]}
            deriving (Eq, Show, Read)

item2ep :: Podcast -> Item -> Episode
item2ep pc item =
    Episode {epId = 0,
             epCast = pc,
             epURL = enclosureurl item,
             epDone = False}

parse :: FilePath -> String -> IO (Either String Feed)
parse fp name = 
    do c <- readFile fp
       case xmlParse' name (unifrob c) of
         Left x -> return (Left x)
         Right y ->
             do let doc = getContent y
                let title = getTitle doc
                let feeditems = getEnclosures doc
                return $ Right $
                           (Feed {channeltitle = title, items = feeditems})
       where getContent (Document _ _ e _) = CElem e

unifrob ('\xef':'\xbb':'\xbf':x) = x -- Strip off unicode BOM
unifrob x = x

unesc = xmlUnEscape stdXmlEscaper

getTitle doc = forceEither $ strofm "title" (channel doc)

getEnclosures doc =
    concat . map procitem $ item doc
    where procitem i = map (procenclosure title guid) enclosure
              where title = case strofm "title" [i] of
                              Left x -> "Untitled"
                              Right x -> x
                    guid = case strofm "guid" [i] of
                              Left _ -> Nothing
                              Right x -> Just x
                    enclosure = tag "enclosure" `o` children $ i
          procenclosure title guid e =
              Item {itemtitle = title,
                    itemguid = guid,
                    enclosureurl = head0 $ forceMaybe $ stratt "url" e,
                    enclosuretype = head0 $ case stratt "type" e of
                                              Nothing -> ["application/octet-stream"]
                                              Just x -> x,
                    enclosurelength = head $ case stratt "length" e of
                                                Nothing -> ["0"]
                                                Just [] -> ["0"]
                                                Just x -> x
                                                }
          head0 [] = ""
          head0 (x:xs) = x
              

item = tag "item" `o` children `o` channel

channel =
    tag "channel" `o` children `o` tag "rss"


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

