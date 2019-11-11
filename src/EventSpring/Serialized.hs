{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
module EventSpring.Serialized where

import           Data.ByteString.Lazy      as BS
import           Data.ByteString.Lazy.UTF8 as BSU

import           Data.Hashable             (Hashable, hash, hashWithSalt)
import qualified Data.List                 as L
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

newtype SerializedType = SerializedType String
    deriving (Eq, Ord, Show)

data PartialDeserialized = PartialDeserialized SerializedType BS.ByteString
    deriving (Eq, Show)

data TypeMismatch = TypeMismatch {
    expectedType :: String,
    actualType   :: String
} deriving (Eq, Show)

data ExtractResult a = ExtractOk a | ExtractError String | ExtractTypeMismatch TypeMismatch

deriving instance Eq a => Eq (ExtractResult a)
deriving instance Show a => Show (ExtractResult a)

deserializeAny :: BS.ByteString -> PartialDeserialized
deserializeAny raw = PartialDeserialized (SerializedType typ) dat
    where
        dat = BS.drop 1 rawDat
        typ = toString rawTyp
        (rawTyp, rawDat) = BS.break (== 0) raw

extractPartial :: forall a. Serialized a => PartialDeserialized -> ExtractResult a
extractPartial (PartialDeserialized (SerializedType typ) dat) =  if typ == expectedType
        then toExtractResult $ deserialize dat
        else ExtractTypeMismatch TypeMismatch { expectedType = expectedType, actualType = typ }
    where
        expectedType = show $ typeRep (Proxy :: Proxy a)
        toExtractResult (Left err) = ExtractError err
        toExtractResult (Right a)  = ExtractOk a


newtype TypeLookup a = TypeLookup [(SerializedType, BS.ByteString -> Either String a)]

instance Semigroup (TypeLookup a) where
    (TypeLookup a) <> (TypeLookup b) = TypeLookup $ a <> b
instance Monoid (TypeLookup a) where
    mempty = TypeLookup []

mkLookup :: forall a. Serialized a => TypeLookup a
mkLookup = TypeLookup [(typeName, deserialize)]
    where
        typeName = SerializedType $ show $ typeRep (Proxy :: Proxy a)

toAnyLookup :: Serialized a => TypeLookup a -> TypeLookup AnySerialized
toAnyLookup (TypeLookup elems) = TypeLookup $ convert <$> elems
    where
        convert (typeName, deserialize) = (typeName, fmap AnySerialized <$> deserialize)

lookupType :: TypeLookup a -> SerializedType -> BS.ByteString -> Either String a
lookupType (TypeLookup elems) typ = \binary -> resDeserialize binary
    where
        resDeserialize bs  = case foundDeserialize of
            Nothing -> Left $ "Not found in lookup, available types: " ++ show (fst <$> elems)
            (Just d) -> d bs
        foundDeserialize = L.lookup typ elems
