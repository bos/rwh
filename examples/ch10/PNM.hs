module PNM
    (
      Anymap(..)
    , Greymap(..)
    ) where

{-- snippet imports --}
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (isSpace)
{-- /snippet imports --}
import Debug.Trace

{-- snippet Anymap --}
class Anymap a where
    anyWidth :: a -> Int
    anyHeight :: a -> Int
    anyParse :: L.ByteString -> [a]
    anyReadFile :: FilePath -> IO [a]
{-- /snippet Anymap --}

{-- snippet Greymap --}
data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)
{-- /snippet Greymap --}

{-- snippet Show --}
instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m
{-- /snippet Show --}

{-- snippet parseP5.type --}
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
{-- /snippet parseP5.type --}

{-- snippet parseP5 --}
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString

-- "nat" here is short for "natural number", not "nathan torkington"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)

parseP5 s =
  case matchHeader (L.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)
{-- /snippet parseP5 --}

{-- snippet parseP5.functions --}
matchHeader h s
    | h `L.isPrefixOf` s = Just (L.dropWhile isSpace (L.drop (L.length h) s))
    | otherwise = Nothing

getNat s = case L.readInt s of
             Nothing -> Nothing
             Just (i, s') | i <= 0    -> Nothing
                          | otherwise -> Just (fromIntegral i, s')

getBytes n s = let n' = fromIntegral n
                   ht@(h, t) = L.splitAt n' s
               in if L.length h < n'
                  then Nothing
                  else Just ht
{-- /snippet parseP5.functions --}

{-- snippet then --}
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f  = f v
{-- /snippet then --}

{-- snippet parseP5_take2 --}
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L.pack "P5") s       >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L.dropWhile isSpace s)
{-- /snippet parseP5_take2 --}

parseAllP5 :: L.ByteString -> [Greymap]
parseAllP5 s = case parseP5_take2 s of
                 Nothing -> []
                 Just (g, s') -> g : parseAllP5 s'
