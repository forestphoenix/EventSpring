{-# LANGUAGE ExistentialQuantification #-}
module EventSpring.Common (
    AnyEvent(..),

    ProjectionVersion,
    versionZero,
    mkVersion,
) where

import           Data.ByteString                 (ByteString)
import           Data.Hashable                   (Hashable, hashWithSalt)
import           Data.Maybe                      (fromMaybe)
import           Data.Typeable                   (TypeRep, Typeable, cast, typeOf)

import           EventSpring.Serialized


newtype ProjectionVersion = ProjectionVersion Int
    deriving (Eq, Ord, Show)

versionZero :: ProjectionVersion
versionZero = ProjectionVersion 0

mkVersion :: Int -> ProjectionVersion
mkVersion = ProjectionVersion

data AnyEvent = forall event. Serialized event => AnyEvent event

instance Eq AnyEvent where
    (AnyEvent id1) == (AnyEvent id2) = fromMaybe False sameValues
        where
            sameValues = (id1 ==) <$> cast id2

instance Show AnyEvent where
    show (AnyEvent c) = "AnyEvent {-" ++ show (typeOf c) ++ "-} " ++ show (serialize c)
