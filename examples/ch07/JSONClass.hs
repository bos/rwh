{-- snippet LANGUAGE --}
{-# LANGUAGE TypeSynonymInstances #-}
{-- /snippet LANGUAGE --}

module JSONClass
    (
      JSON(..)
    , JValue(..)
    , JAry
    , JObj
    , fromJAry
    , jarray
    , fromJObj
    , jobject
    ) where

import Control.Arrow (second)
import Control.Monad (liftM)

newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)

jarray :: JSON a => [a] -> JAry a
jarray = JAry

newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

jobject :: JSON a => [(String, a)] -> JObj a
jobject = JObj

data JValue = JString String
            | JNumber Double
            | JObject (JObj JValue)
            | JArray (JAry JValue)
            | JBool !Bool
            | JNull
              deriving (Eq, Ord, Show)

{-- snippet class --}
type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right
{-- /snippet class --}

{-- snippet String --}
instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"
{-- /snippet String --}

{-- snippet doubleToJValue --}
doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id
{-- /snippet doubleToJValue --}

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

instance (JSON a) => JSON (JAry a) where
    toJValue = JArray . jarray . map toJValue . fromJAry
    fromJValue (JArray a) = whenRight jarray (mapEithers fromJValue (fromJAry a))
    fromJValue _ = Left "not a JSON array"

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . jobject . map (second toJValue) . fromJObj
    fromJValue (JObject o) = whenRight jobject (mapEithers unwrap (fromJObj o))
        where unwrap (k, v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"

{-- snippet Bool --}
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"
{-- /snippet Bool --}

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

instance (JSON a) => JSON (Maybe a) where
    toJValue = maybe JNull toJValue
    fromJValue JNull = Right Nothing
    fromJValue v = whenRight Just (fromJValue v)
