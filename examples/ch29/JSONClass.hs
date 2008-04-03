{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JSONClass
    (
      JSON(..)
    , JValue(..)
    , JArray
    , JObject
    , fromJArray
    , jarray
    , fromJObject
    , jobject
    ) where

import Control.Arrow (second)
import Control.Monad (liftM)

newtype JArray a = JAry {
      fromJArray :: [a]
    } deriving (Eq, Ord, Show)

jarray :: JSON a => [a] -> JArray a
jarray = JAry

newtype JObject a = JObj {
      fromJObject :: [(String, a)]
    } deriving (Eq, Ord, Show)

jobject :: JSON a => [(String, a)] -> JObject a
jobject = JObj

data JValue = JString String
            | JNumber !Rational
            | JObject (JObject JValue)
            | JArray (JArray JValue)
            | JBool !Bool
            | JNull
              deriving (Eq, Ord, Show)

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: Monad m => JValue -> m a

instance JSON JValue where
    toJValue = id
    fromJValue = return

rationalToJValue :: (Monad m) => (Rational -> b) -> JValue -> m b
rationalToJValue f (JNumber v) = return . f $ v
rationalToJValue _ _ = fail "not a JSON number"

instance JSON String where
    toJValue = JString
    fromJValue (JString s) = return s
    fromJValue _ = fail "not a JSON string"

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

instance (JSON a) => JSON (JArray a) where
    toJValue = JArray . jarray . map toJValue . fromJArray
    fromJValue (JArray a) = jarray `liftM` mapM fromJValue (fromJArray a)
    fromJValue _ = fail "not a JSON array"

instance (JSON a) => JSON (JObject a) where
    toJValue = JObject . jobject . map (second toJValue) . fromJObject
    fromJValue (JObject o) = jobject `liftM` mapM unwrap (fromJObject o)
        where unwrap (k, v) = fromJValue v >>= return . (,) k
    fromJValue _ = fail "not a JSON object"

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = return b
    fromJValue _ = fail "not a JSON boolean"

instance (JSON a) => JSON (Maybe a) where
    toJValue = maybe JNull toJValue
    fromJValue JNull = return Nothing
    fromJValue v = Just `liftM` fromJValue v
