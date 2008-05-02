{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module BrokenClass
    (
      JSON(..)
    , JValue(..)
    ) where

import SimpleJSON
import Control.Arrow (second)
import Control.Monad (liftM)

{-- snippet class --}
type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a
{-- /snippet class --}

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue (JNumber v) = Right (round v)
    fromJValue _ = Left "not a JSON number"

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

{-- snippet array --}
instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined
{-- /snippet array --}

{-- snippet object --}
instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined
{-- /snippet object --}

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

{-
instance (JSON a) => JSON [a] where
    toJValue = JArray . map toJValue
    fromJValue (JArray a) = mapEithers fromJValue a
    fromJValue _ = Left "not a JSON array"

instance (JSON a) => JSON [(String, a)] where
    toJValue = JObject . map (second toJValue)
    fromJValue (JObject o) = mapEithers unwrap o
        where unwrap (k, v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"
-}

{-
instance (JSON a, JSON b) => JSON (a, b) where
    toJValue (a, b) = JArray [toJValue a, toJValue b]
    fromJValue (JArray [a,b]) = undefined
    fromJValue _ = Left "not a JSON array"
-}
