{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

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
            | JNumber !Rational
            | JObject (JObj JValue)
            | JArray (JAry JValue)
            | JBool !Bool
            | JNull
              deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

rationalToJValue :: (Rational -> a) -> JValue -> Either JSONError a
rationalToJValue f (JNumber v) = Right (f v)
rationalToJValue _ _ = Left "not a JSON number"

instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"

instance JSON Int where
    toJValue = JNumber . toRational
    fromJValue = rationalToJValue round

instance JSON Integer where
    toJValue = JNumber . toRational
    fromJValue = rationalToJValue round

instance JSON Double where
    toJValue = JNumber . toRational
    fromJValue = rationalToJValue fromRational

instance JSON Rational where
    toJValue = JNumber
    fromJValue = rationalToJValue id

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

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

instance (JSON a) => JSON (Maybe a) where
    toJValue = maybe JNull toJValue
    fromJValue JNull = Right Nothing
    fromJValue v = whenRight Just (fromJValue v)
