{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
module EventSpring.Serialized where

import           Data.ByteString.Lazy      as BS
import           Data.ByteString.Lazy.UTF8 as BSU

import           Data.Hashable             (Hashable, hash, hashWithSalt)
import           Data.Maybe                (maybe)
import           Data.Semigroup            ((<>))
import           Data.Typeable             (Proxy (..), TypeRep, Typeable, cast,
                                            typeOf, typeRep)

class (Typeable content, Show content, Eq content) => Serialized content where
    serialize :: content -> BS.ByteString
    deserialize :: BS.ByteString -> Either String content


data AnySerialized = forall a. Serialized a => AnySerialized a

instance Eq AnySerialized where
    (AnySerialized a) == (AnySerialized b) = maybe False (a ==) bAsA
        where
            bAsA = cast b

instance Show AnySerialized where
    show (AnySerialized c) = "AnySerialized {-" ++ show (typeOf c) ++ "-} " ++ show (show c)

castAny :: Serialized a => AnySerialized -> Maybe a
castAny (AnySerialized a) = cast a

serializeAny :: AnySerialized -> BS.ByteString
serializeAny (AnySerialized dat) =
    BSU.fromString (show $ typeOf dat) <>
    BS.singleton 0 <>
    serialize dat


data AnyHashable = forall a. (Serialized a, Hashable a) => AnyHashable a

instance Show AnyHashable where
    show (AnyHashable c) = "AnyHashable {-" ++ show (typeOf c) ++ " hash:" ++ show (hash c) ++ "-} " ++ show (show c)

instance Eq AnyHashable where
    (AnyHashable a) == (AnyHashable b) = maybe False (a ==) bAsA
        where
            bAsA = cast b

instance Hashable AnyHashable where
    hashWithSalt seed (AnyHashable a) = hashWithSalt seed a
    hash (AnyHashable a) = hash a

castHashable :: Serialized a => AnyHashable -> Maybe a
castHashable (AnyHashable a) = cast a

serializeHashable :: AnyHashable -> BS.ByteString
serializeHashable (AnyHashable dat) =
    BSU.fromString (show $ typeOf dat) <>
    BS.singleton 0 <>
    serialize dat

data PartialDeserialized = PartialDeserialized String BS.ByteString
    deriving Show

data TypeMismatch = TypeMismatch {
    expectedType :: String,
    actualType   :: String
} deriving (Eq, Show)

data ExtractResult a = ExtractOk a | ExtractError String | ExtractTypeMismatch TypeMismatch

deriving instance Eq a => Eq (ExtractResult a)
deriving instance Show a => Show (ExtractResult a)

deserializeAny :: BS.ByteString -> PartialDeserialized
deserializeAny raw = PartialDeserialized typ dat
    where
        dat = BS.drop 1 rawDat
        typ = toString rawTyp
        (rawTyp, rawDat) = BS.break (== 0) raw

extractPartial :: forall a. Serialized a => PartialDeserialized -> ExtractResult a
extractPartial (PartialDeserialized typ dat) =  if typ == expectedType
        then toExtractResult $ deserialize dat
        else ExtractTypeMismatch TypeMismatch { expectedType = expectedType, actualType = typ }
    where
        expectedType = show $ typeRep (Proxy :: Proxy a)
        toExtractResult (Left err) = ExtractError err
        toExtractResult (Right a)  = ExtractOk a
