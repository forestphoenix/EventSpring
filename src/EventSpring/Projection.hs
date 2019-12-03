{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
module EventSpring.Projection (
    ProjectionFor,
    Event,
    Projection,
    ProjId,

    OnEvent(..),
    Delta(..),
    Projector(..),

    AnyProjId(..),
    mkAnyProjId,
    onAnyProjId,
    AnyProjection(..),
    mkAnyProjection,

    toAnyDelta,
    toAnyOnEvent,
    toAnyProjector,

    -- Projecting
    deltasForEvent,
    idFromDelta,
    applyDelta,

    -- Projecting for Unit-tests
    applyDeltaAL,
    runEventAL,
) where

import           Control.Arrow          (second)
import           Data.ByteString        (ByteString)
import           Data.Hashable          (Hashable, hash, hashWithSalt)
import           Data.List              (partition)
import           Data.Maybe             (listToMaybe, maybeToList)
import           Data.Semigroup         (Semigroup)
import           Data.Traversable       (for)
import           Data.Typeable          (TypeRep, Typeable, cast, typeOf)

import           EventSpring.Serialized

type family ProjectionFor projId

class Serialized event => Event event

class Serialized proj => Projection proj

class (Serialized projId, Hashable projId) => ProjId projId

data OnEvent onEvent = forall event. Serialized event =>
    OnEvent (event -> onEvent)

instance Functor OnEvent where
    fmap f (OnEvent g) = OnEvent $ f . g

instance (Show a, Typeable a) => Show (OnEvent a) where
    show (OnEvent f) = "OnEvent (f :: " ++ show (typeOf f) ++ ")"

-- Single-Projection projectors

data Delta projId =
    -- | Update the projection with id 'projId'. This does nothing if the projection does not exist
    Update projId (ProjectionFor projId -> ProjectionFor projId) |
    -- | Create the prjection with id 'projId'.
    --   If the projection already exists, this behaves like 'Update projId (const <value>)'
    Create projId (ProjectionFor projId) |
    -- | Creates the prjection with id 'projId' or updates it if it already exists.
    CreateOrUpdate projId (ProjectionFor projId) (ProjectionFor projId -> ProjectionFor projId)

instance (Show i) => Show (Delta i) where
    show (Update i f)           = "Update " ++ show i ++ " f"
    show (Create i v)           = "Create" ++ show i ++ " v"
    show (CreateOrUpdate i v f) = "CreateOrUpdate " ++ show i ++ " v f"

newtype Projector projId = Projector [OnEvent [Delta projId]]
    deriving (Semigroup, Monoid)

-- Any-Projection Projectors

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

onAnyProjId :: AnyProjId -> (forall h. (ProjId h, Projection (ProjectionFor h)) => h -> o) -> o
onAnyProjId (AnyProjId d) f = f d


newtype AnyProjection = AnyProjection { unAnyProjection :: AnySerialized }
    deriving (Eq, Show)

mkAnyProjection :: Projection a => a -> AnyProjection
mkAnyProjection = AnyProjection . AnySerialized

type instance ProjectionFor AnyProjId = AnyProjection

toAnyFunction :: Serialized a => (a -> a) -> AnySerialized -> AnySerialized
toAnyFunction f a = case castAny a of
                        Nothing  -> error "todo"
                        (Just a) -> AnySerialized $ f a

toAnyDelta :: (ProjId a, Projection (ProjectionFor a)) => Delta a -> Delta AnyProjId
toAnyDelta (Create i v) = Create (AnyProjId i) (AnyProjection $ AnySerialized v)
toAnyDelta (Update i f) = Update (AnyProjId i) (AnyProjection . toAnyFunction f . unAnyProjection)
toAnyDelta (CreateOrUpdate i v f) = CreateOrUpdate (AnyProjId i)
                                                   (AnyProjection $ AnySerialized v)
                                                   (AnyProjection . toAnyFunction f . unAnyProjection)

toAnyOnEvent :: (ProjId a, Projection (ProjectionFor a)) => OnEvent [Delta a] -> OnEvent [Delta AnyProjId]
toAnyOnEvent = fmap (fmap toAnyDelta)

toAnyProjector :: (ProjId a, Projection (ProjectionFor a)) => Projector a -> Projector AnyProjId
toAnyProjector (Projector oes) = Projector $ toAnyOnEvent <$> oes

-- Projecting

deltasForEvent :: (Serialized event, Typeable projId, Show projId) => Projector projId -> event -> [Delta projId]
deltasForEvent (Projector handlers) event =
    concat $ (\(OnEvent handler) ->
        case cast event of
            Nothing  -> []
            (Just e) -> handler e
    ) <$> handlers

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

runEventAL :: (Event event, ProjId projId, Projection (ProjectionFor projId)) =>
    Projector projId -> event -> [(projId, ProjectionFor projId)] -> [(projId, ProjectionFor projId)]
runEventAL projector event = foldl (.) id (applyDeltaAL <$> (deltasForEvent projector event))
