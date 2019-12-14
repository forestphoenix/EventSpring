{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module EventSpring.Common where

import           Data.ByteString        (ByteString)
import           Data.Hashable          (Hashable, hash, hashWithSalt)
import           Data.Maybe             (fromMaybe)
import           Data.Typeable          (TypeRep, Typeable, cast, typeOf)

import           EventSpring.Serialized


newtype ProjectionVersion = ProjectionVersion Int
    deriving (Eq, Ord, Show)

versionZero :: ProjectionVersion
versionZero = ProjectionVersion 0

mkVersion :: Int -> ProjectionVersion
mkVersion = ProjectionVersion

incrementVersion :: ProjectionVersion -> ProjectionVersion
incrementVersion (ProjectionVersion v) = ProjectionVersion $ v + 1


type family ProjectionFor projId

class Serialized event => Event event

class Serialized proj => Projection proj

class (Serialized projId, Hashable projId) => ProjId projId


data AnyEvent = forall event. Event event => AnyEvent event

instance Eq AnyEvent where
    (AnyEvent id1) == (AnyEvent id2) = fromMaybe False sameValues
        where
            sameValues = (id1 ==) <$> cast id2

instance Show AnyEvent where
    show (AnyEvent c) = "AnyEvent {-" ++ show (typeOf c) ++ "-} " ++ show (serialize c)


data AnyProjId = forall i. (ProjId i, Projection (ProjectionFor i)) => AnyProjId { unAnyProjId :: i }

instance Show AnyProjId where
    show (AnyProjId c) = "AnyProjId {- " ++ show (typeOf c) ++ " hash:" ++ show (hash c) ++ " -} " ++ show (show c)

instance Eq AnyProjId where
    (AnyProjId a) == (AnyProjId b) = maybe False (a ==) bAsA
        where
            bAsA = cast b

instance Hashable AnyProjId where
    hashWithSalt seed (AnyProjId a) = hashWithSalt seed a
    hash (AnyProjId a) = hash a

mkAnyProjId :: (ProjId a, Projection (ProjectionFor a)) => a -> AnyProjId
mkAnyProjId = AnyProjId


newtype AnyProjection = AnyProjection { unAnyProjection :: AnySerialized }
    deriving (Eq, Show)

mkAnyProjection :: Projection a => a -> AnyProjection
mkAnyProjection = AnyProjection . AnySerialized

type instance ProjectionFor AnyProjId = AnyProjection
