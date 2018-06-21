{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EventSpring.Serialized where

import           Data.ByteString.Lazy as BS
import           Data.ByteString.Lazy.UTF8 as BSU

import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, Proxy(..), typeRep, typeOf, cast)

class (Typeable content, Eq content) => Serialized content where
    serialize :: content -> BS.ByteString
    deserialize :: BS.ByteString -> Either String content

data AnySerialized = forall a. Serialized a => AnySerialized a

instance Eq AnySerialized where
    (AnySerialized a) == (AnySerialized b) = fromMaybe False (fmap (a ==) bAsA)
        where
            bAsA = cast b

instance Show AnySerialized where
    show (AnySerialized c) = "AnySerialized {-" ++ show (typeOf c) ++ "-} " ++ show (serialize c)

serializeAny :: AnySerialized -> BS.ByteString
serializeAny (AnySerialized dat) =
    BSU.fromString (show $ typeOf dat) <>
    BS.singleton 0 <>
    serialize dat

data PartialDeserialized = PartialDeserialized String BS.ByteString
    deriving Show

deserializeAny :: BS.ByteString -> PartialDeserialized
deserializeAny raw = PartialDeserialized typ dat
    where
        dat = BS.drop 1 rawDat
        typ = toString rawTyp
        (rawTyp, rawDat) = BS.break (== 0) raw

extractPartial :: forall a. Serialized a => PartialDeserialized -> Either String a
extractPartial (PartialDeserialized typ dat) =  if typ == expectedType
        then deserialize dat
        else Left $ "Error in extractPartial: type mismatch (expected " <> expectedType <>
                  ", got " <> typ <> ")"
    where
        expectedType = show $ typeRep (Proxy :: Proxy a)
