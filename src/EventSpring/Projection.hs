{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module EventSpring.Projection (
    ProjectionFor,

    OnEvent(..),
    Delta(..),
    Projector(..),

    AnyDelta(..),
    AnyProjector(),
    toAnyProjector,

    -- Projecting
    deltasForEvent,
    anyDeltasForEvent,

    -- Projecting for Unit-tests
    projectOntoAL,
    AnyProjected(..),
    projectOntoAnyAL,
) where

import           Data.ByteString             (ByteString)
import           Data.Hashable               (Hashable)
import           Data.Typeable               (TypeRep, Typeable)

import           EventSpring.Serialized

type family ProjectionFor projId

data OnEvent onEvent = forall event. Serialized event =>
    OnEvent (event -> onEvent)

instance Functor OnEvent where
    fmap f (OnEvent g) = OnEvent $ f . g

-- Single-Projection projectors

data Delta projId =
    -- | Update the projection with id 'projId'. This does nothing if the projection does not exist
    Update projId (ProjectionFor projId -> ProjectionFor projId) |
    -- | Create the prjection with id 'projId'.
    --   If the projection already exists, this behaves like 'Update projId id'
    Create projId (ProjectionFor projId) |
    -- | Creates the prjection with id 'projId' or updates it if it already exists.
    CreateOrUpdate projId (ProjectionFor projId) (ProjectionFor projId -> ProjectionFor projId)

newtype Projector projId = Projector [OnEvent (Delta projId)]
    deriving (Semigroup, Monoid)

-- Any-Projection Projectors

data AnyDelta = forall projId proj.
    (Serialized projId, Serialized (ProjectionFor projId)) =>
    AnyDelta (Delta projId)

toAnyDelta :: (proj ~ ProjectionFor projId, Serialized projId, Serialized proj) =>
    Delta projId -> AnyDelta
toAnyDelta = AnyDelta

newtype AnyProjector = AnyProjector [OnEvent AnyDelta]
    deriving (Semigroup, Monoid)

toAnyProjector (Projector oes) = AnyProjector $ fmap toAnyDelta <$> oes

-- Projecting

deltasForEvent :: Serialized event => Projector projId -> event -> [Delta projId]
deltasForEvent = undefined

anyDeltasForEvent :: Serialized event => AnyProjector -> event -> [AnyDelta]
anyDeltasForEvent = undefined

-- Projecting for Unit Tests

projectOntoAL :: (Serialized event) =>
    Projector projId -> event -> [(projId, ProjectionFor projId)] -> [(projId, ProjectionFor projId)]
projectOntoAL = undefined

data AnyProjected = forall projId proj.
    (proj ~ ProjectionFor projId, Serialized projId, Serialized proj) =>
    AnyProjected projId proj
-- TODO: Eq, Show

projectOntoAnyAL :: Serialized event => AnyProjector -> event -> [AnyProjected] -> [AnyProjected]
projectOntoAnyAL = undefined
