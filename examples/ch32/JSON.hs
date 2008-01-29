{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JSON
    (
      JSON(..)
    , JValue(..)
    , JObject
    , fromJObject
    , jobject
    ) where

import Control.Arrow (second)
import Control.Monad (liftM)

newtype JObject a = JObj {
      fromJObject :: [(String, a)]
    } deriving (Eq, Ord, Show)

jobject :: JSON a => [(String, a)] -> JObject a
jobject = JObj

data JValue = JString String
            | JNumber !Rational
            | JObject (JObject JValue)
            | JArray [JValue]
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
rationalToJValue _ _ = fail "not a number"

instance JSON String where
    toJValue = JString
    fromJValue (JString s) = return s
    fromJValue _ = fail "not a string"

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

instance (JSON a) => JSON [a] where
    toJValue = JArray . map toJValue
    fromJValue (JArray a) = mapM fromJValue a
    fromJValue _ = fail "not an array"

instance (JSON a) => JSON (JObject a) where
    toJValue = JObject . jobject . map (second toJValue) . fromJObject
    fromJValue (JObject o) = jobject `liftM` mapM unwrap (fromJObject o)
        where unwrap (k, v) = fromJValue v >>= return . (,) k
    fromJValue _ = fail "not an object"

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = return b
    fromJValue _ = fail "not a boolean"

instance (JSON a) => JSON (Maybe a) where
    toJValue = maybe JNull toJValue
    fromJValue JNull = return Nothing
    fromJValue v = Just `liftM` fromJValue v
