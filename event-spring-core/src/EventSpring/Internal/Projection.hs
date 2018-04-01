{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeOperators             #-}
module EventSpring.Internal.Projection where

import           Data.ByteString                 (ByteString)
import           Data.Typeable                   (TypeRep, Typeable)

import           EventSpring.Internal.Common
import           EventSpring.Serialized

class IsProjectionIdFor projId projValue | projId -> projValue

newtype Projections = Projections [Projection]

data Projection = forall projId projValue. (
    Serialized projId,
    Serialized projValue,
    projId `IsProjectionIdFor` projValue
    ) => Projection {
    initialValue    :: projValue,
    transformations :: [OnEvent projId projValue]
}

data OnEvent projId projValue =
    forall event. Serialized event =>
        Transformation (event -> Change projId projValue)

data Change projId projValue = Delta projId (projValue -> projValue)



data Update = forall projId proj. (projId `IsProjectionIdFor` proj) => Update projId (proj -> proj)

class CanHandleProjection projector projection where
    initialProjection :: projector -> projection

class CanHandleEvent projector event where
    changesForEvent :: projector -> event -> [Update]
