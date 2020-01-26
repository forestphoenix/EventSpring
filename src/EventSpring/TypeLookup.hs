{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EventSpring.TypeLookup where

import           EventSpring.Common
import           EventSpring.Projection
import           EventSpring.Serialized

import           Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Strict    as M
import           Data.Typeable          (Proxy (..), typeRep)

newtype TypeLookup a = TypeLookup (M.HashMap SerializedType (BS.ByteString -> Either String a))

instance Semigroup (TypeLookup a) where
    (TypeLookup a) <> (TypeLookup b) = TypeLookup $ a <> b
instance Monoid (TypeLookup a) where
    mempty = TypeLookup M.empty
instance Functor TypeLookup where
    fmap f (TypeLookup m) = TypeLookup $ (fmap (fmap f)) <$> m

mkLookup :: forall a. Serialized a => TypeLookup a
mkLookup = TypeLookup $ M.singleton typeName deserialize
    where
        typeName = SerializedType $ show $ typeRep (Proxy :: Proxy a)

toEventLookup :: Event a => TypeLookup a -> TypeLookup AnyEvent
toEventLookup (TypeLookup elems) = TypeLookup $ convert <$> elems
    where
        convert deserialize = fmap AnyEvent <$> deserialize

lookupType :: TypeLookup a -> SerializedType -> BS.ByteString -> Either String a
lookupType (TypeLookup elems) typ = \binary -> resDeserialize binary
    where
        resDeserialize bs  = case foundDeserialize of
            Nothing -> Left $ "Not found in lookup, available types: " ++ show (M.keys elems)
            (Just d) -> d bs
        foundDeserialize = M.lookup typ elems

-- Deserializing from projectors

getDeserializer :: forall a. OnEvent a -> (SerializedType, BS.ByteString -> Either String AnyEvent)
getDeserializer (OnEvent f) = toAny $ deserializerFromDomain f
    where
        toAny (a, b) = (a, fmap (fmap AnyEvent) b)

deserializerFromDomain :: forall a b. Serialized a => (a -> b) -> (SerializedType, BS.ByteString -> Either String a)
deserializerFromDomain _ = (serializedType, deserialize)
    where
        serializedType = SerializedType $ show $ (typeRep (Proxy :: Proxy a))

lookupFromProjector :: Projector a -> TypeLookup AnyEvent
lookupFromProjector (Projector onEvents) = TypeLookup $ M.fromList $ getDeserializer <$> onEvents
