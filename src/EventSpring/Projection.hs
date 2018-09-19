{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module EventSpring.Projection (
    ProjectionFor,

    OnEvent(..),
    Delta(..),
    Projector(..),
    
    AnyProjId(..),
    mkAnyProjId, 
    AnyProjection(..),
    mkAnyProjection, 

    toAnyDelta,
    toAnyProjector,

    -- Projecting
    deltasForEvent,

    -- Projecting for Unit-tests
    applyDeltaAL,
    runEventAL,
) where

import           Control.Arrow          (second)
import           Data.ByteString        (ByteString)
import           Data.Hashable          (Hashable)
import           Data.Semigroup         (Semigroup)
import           Data.List              (partition)
import           Data.Typeable          (TypeRep, Typeable, cast)
import           Data.Maybe             (maybeToList, listToMaybe)

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
    --   If the projection already exists, this behaves like 'Update projId (const <value>)'
    Create projId (ProjectionFor projId) |
    -- | Creates the prjection with id 'projId' or updates it if it already exists.
    CreateOrUpdate projId (ProjectionFor projId) (ProjectionFor projId -> ProjectionFor projId)

newtype Projector projId = Projector [OnEvent [Delta projId]]
    deriving (Semigroup, Monoid)

-- Any-Projection Projectors

newtype AnyProjId = AnyProjId { unAnyProjId :: AnyHashable }
    deriving (Eq, Show, Hashable)

mkAnyProjId :: (Serialized a, Hashable a) => a -> AnyProjId
mkAnyProjId = AnyProjId . AnyHashable

newtype AnyProjection = AnyProjection { unAnyProjection :: AnySerialized }
    deriving (Eq, Show)

mkAnyProjection :: Serialized a => a -> AnyProjection
mkAnyProjection = AnyProjection . AnySerialized

type instance ProjectionFor AnyProjId = AnyProjection

toAnyFunction :: Serialized a => (a -> a) -> AnySerialized -> AnySerialized
toAnyFunction f a = case castAny a of
                        Nothing  -> error "todo"
                        (Just a) -> AnySerialized $ f a

toAnyDelta :: (Serialized a, Eq a, Hashable a, Serialized (ProjectionFor a)) => Delta a -> Delta AnyProjId
toAnyDelta (Create i v) = Create (AnyProjId $ AnyHashable i) (AnyProjection $ AnySerialized v)
toAnyDelta (Update i f) = Update (AnyProjId  $ AnyHashable i) (AnyProjection . toAnyFunction f . unAnyProjection)
toAnyDelta (CreateOrUpdate i v f) = CreateOrUpdate (AnyProjId $ AnyHashable i)
                                                   (AnyProjection $ AnySerialized v)
                                                   (AnyProjection . toAnyFunction f . unAnyProjection)

toAnyProjector :: (Serialized a, Hashable a, Serialized (ProjectionFor a)) => Projector a -> Projector AnyProjId 
toAnyProjector (Projector oes) = Projector $ fmap (fmap toAnyDelta) <$> oes

-- Projecting

deltasForEvent :: Serialized event => Projector projId -> event -> [Delta projId]
deltasForEvent = undefined

-- Projecting for Unit Tests

partitionByKey :: Eq a => a -> [(a, b)] -> ([(a, b)], [(a, b)])
partitionByKey key = partition ((== key) . fst)

applyDelta :: Delta projId -> Maybe (projId, ProjectionFor projId) -> Maybe (projId, ProjectionFor projId)
applyDelta (Create pId newVal)           = const $ Just (pId, newVal)
applyDelta (Update pId f)                = fmap $ second f
applyDelta (CreateOrUpdate pId newVal f) = Just . maybe (pId, newVal) (second f)

idFromDelta :: Delta projId -> projId
idFromDelta (Create i _)           = i
idFromDelta (Update i _)           = i
idFromDelta (CreateOrUpdate i _ _) = i

applyDeltaAL :: Eq projId =>
    Delta projId ->
    [(projId, ProjectionFor projId)] ->
    [(projId, ProjectionFor projId)]
applyDeltaAL delta al = maybeToList newVal ++ others
    where
        newVal = applyDelta delta $ listToMaybe withId
        (withId, others) = partitionByKey (idFromDelta delta) al

runEventAL :: (Serialized event) =>
    Projector projId -> event -> [(projId, ProjectionFor projId)] -> [(projId, ProjectionFor projId)]
runEventAL = undefined

