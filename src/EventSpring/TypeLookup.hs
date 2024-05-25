{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EventSpring.TypeLookup where

import           EventSpring.Common
import           EventSpring.Projection
import           EventSpring.Serialized

import           Control.Monad.Catch    (Exception)
import           Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Strict    as M
import           Data.Typeable          (Proxy (..), typeRep)
import           Data.Bifunctor         (first)

newtype TypeLookup a = TypeLookup (M.HashMap SerializedType (BS.ByteString -> Either DeserializeFailed a))

instance Semigroup (TypeLookup a) where
    (TypeLookup a) <> (TypeLookup b) = TypeLookup $ a <> b
instance Monoid (TypeLookup a) where
    mempty = TypeLookup M.empty
instance Functor TypeLookup where
    fmap f (TypeLookup m) = TypeLookup $ (fmap (fmap f)) <$> m


data DeserializeFailed = TypeLookupFailed SerializedType [SerializedType] | BinaryDeserializeFailed String
    deriving (Show)

instance Exception DeserializeFailed

convertedDeserialize :: (Serialized content) =>  BS.ByteString -> Either DeserializeFailed content
convertedDeserialize bytes = first BinaryDeserializeFailed $ deserialize bytes

mkLookup :: forall a. Serialized a => TypeLookup a
mkLookup = TypeLookup $ M.singleton typeName convertedDeserialize
    where
        typeName = SerializedType $ show $ typeRep (Proxy :: Proxy a)

toEventLookup :: Event a => TypeLookup a -> TypeLookup AnyEvent
toEventLookup (TypeLookup elems) = TypeLookup $ convert <$> elems
    where
        convert deserialize = fmap AnyEvent <$> deserialize

lookupType :: TypeLookup a -> SerializedType -> BS.ByteString -> Either DeserializeFailed a
lookupType (TypeLookup elems) typ = \binary -> resDeserialize binary
    where
        resDeserialize bs  = case foundDeserialize of
            Nothing -> Left $ TypeLookupFailed typ (M.keys elems)
            (Just d) -> d bs
        foundDeserialize = M.lookup typ elems

-- Deserializing from projectors

getDeserializer :: forall a. OnEvent a -> (SerializedType, BS.ByteString -> Either DeserializeFailed AnyEvent)
getDeserializer (OnEvent f) = toAny $ deserializerFromDomain f
    where
        toAny (a, b) = (a, fmap (fmap AnyEvent) b)

deserializerFromDomain :: forall a b. Serialized a => (a -> b) -> (SerializedType, BS.ByteString -> Either DeserializeFailed a)
deserializerFromDomain _ = (serializedType, convertedDeserialize)
    where
        serializedType = SerializedType $ show $ (typeRep (Proxy :: Proxy a))

lookupFromProjector :: Projector a -> TypeLookup AnyEvent
lookupFromProjector (Projector onEvents) = TypeLookup $ M.fromList $ getDeserializer <$> onEvents
