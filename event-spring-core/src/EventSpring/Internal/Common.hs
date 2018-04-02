{-# LANGUAGE ExistentialQuantification #-}
module EventSpring.Internal.Common where

import           Data.ByteString                 (ByteString)
import           Data.Hashable                   (Hashable, hashWithSalt)
import           Data.Maybe                      (fromMaybe)
import           Data.Typeable                   (TypeRep, Typeable, cast)

import           EventSpring.Serialized


data AnyProjection = forall proj. Serialized proj => AnyProjection proj

data AnyProjectionId = forall projId. (Serialized projId, Eq projId, Hashable projId) =>
    AnyProjectionId projId

instance Eq AnyProjectionId where
    (AnyProjectionId id1) == (AnyProjectionId id2) = fromMaybe False sameValues
        where
            sameValues = (id1 ==) <$> cast id2

instance Hashable AnyProjectionId where
    hashWithSalt salt (AnyProjectionId pId) = hashWithSalt salt pId

newtype ProjectionVersion = ProjectionVersion Int

versionZero :: ProjectionVersion
versionZero = ProjectionVersion 0


data AnyEvent = forall event. Serialized event => AnyEvent event
