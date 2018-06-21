{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts #-}
module EventSpring.TransactionT (
    TransactionContext,
    mkTransactionContext,

    TransactionT,
    runTransactionT,
    readProjection,
    recordSingle,
    record,

    TransactionResult(..),
    NewProjection(..),
    ReadProjection(..)
) where

import           Control.Applicative
import           Control.Monad                   (forM, forM_)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.RWS.Strict
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as M
import           Data.Maybe                      (fromMaybe, maybe)
import qualified Data.Sequence                   as S
import           Data.Typeable                   (cast, typeOf)

import           EventSpring.Common
import           EventSpring.Projection
import           EventSpring.Serialized

type ProjReader m = forall projId.
    (Serialized projId, Serialized (ProjectionFor projId)) =>
    projId -> m (Maybe (ProjectionVersion, ProjectionFor projId))

data NewProjection = forall projId.
    (Serialized projId, Serialized (ProjectionFor projId)) =>
    NewProjection projId (ProjectionFor projId)

instance Eq NewProjection where
    (NewProjection p1 v1) == (NewProjection p2 v2) = fromMaybe False $
        (&&) <$> samePs <*> sameVs
        where
            samePs = (p1 ==) <$> cast p2
            sameVs = (v1 ==) <$> cast v2

instance Show NewProjection where
    show (NewProjection p v) =
        "NewProjection " ++
        "{-" ++ show (typeOf p) ++ "-} " ++ show (serialize p) ++
        "{-" ++ show (typeOf v) ++ "-} " ++ show (serialize v)

data ReadProjection = forall projId proj. (
    Eq projId, Serialized projId
    ) =>
    ReadProjection projId ProjectionVersion

instance Eq ReadProjection where
    (ReadProjection p1 v1) == (ReadProjection p2 v2) = v1 == v2 && fromMaybe False sameValues
        where
            sameValues = (p1 ==) <$> cast p2
instance Show ReadProjection where
    show (ReadProjection p v) =
        "ReadProjection " ++ show v ++
        " {-" ++ show (typeOf p) ++ "-} " ++ show (serialize p)

data TransactionContext md m = TransactionContext {
    tcReadProjection :: ProjReader m,

    tcProjector      :: AnyProjector,

    tcMetadata       :: md
}

data TransactionState = TransactionState {
    tsNewEvents       :: S.Seq AnyEvent,
    tsReadProjections :: M.HashMap AnyProjectionId (ProjectionVersion, AnyProjection),
    tsNewProjections  :: M.HashMap AnyProjectionId NewProjection
}

data TransactionResult = TransactionResult {
    trNewEvents :: S.Seq AnyEvent,
    trNewProjs  :: [NewProjection],
    trReadProjs :: [ReadProjection]
}

newtype TransactionT md m a = TransactionT
    { unTransactionT :: (RWST (TransactionContext md m) () TransactionState m a) }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (TransactionT md) where
    lift m = TransactionT $ lift m

readCachedProjection :: forall proj projId. Serialized proj =>
    TransactionState -> AnyProjectionId -> Maybe proj
readCachedProjection TransactionState{..} projId =
    (castNewProj =<< M.lookup projId tsNewProjections) <|>
    ((castToProj . snd) =<< M.lookup projId tsReadProjections)
    where
        castToProj (AnyProjection proj) = (cast proj) :: Maybe proj
        castNewProj (NewProjection _ proj) = cast proj

loadProjection :: (
    Serialized projId, Serialized (ProjectionFor projId),
    Monad m) =>
    projId -> TransactionT md m (Maybe (ProjectionVersion, ProjectionFor projId))
loadProjection projId = TransactionT $ do
    reader <- tcReadProjection <$> ask
    loaded <- lift $ reader projId
    pure $ loaded

cacheProjection :: (
    Serialized projId, Hashable projId, Serialized (ProjectionFor projId),
    Monad m) =>
    projId -> ProjectionVersion -> (ProjectionFor projId) -> TransactionT md m ()
cacheProjection projId ver val = TransactionT $
    modify $ \state@TransactionState{..} -> state {
        tsReadProjections = M.insert (AnyProjectionId projId) (ver, AnyProjection val) tsReadProjections
    }

readProjection :: (
    Serialized projId, Hashable projId, Serialized (ProjectionFor projId),
    Monad m) =>
    projId -> TransactionT md m (Maybe (ProjectionFor projId))
readProjection projId = TransactionT $ do
    trState <- get
    projector <- tcProjector <$> ask
    let cached = readCachedProjection trState $ AnyProjectionId projId
    case cached of
        Just p  -> pure $ Just p
        Nothing -> do
             loaded <- unTransactionT $ loadProjection projId
             case loaded of
                 Nothing -> pure Nothing
                 Just (readVer, readProj) -> do
                     unTransactionT $ cacheProjection projId readVer readProj
                     return $ Just readProj

recordSingle :: (
    Serialized e,
    Monad m
    ) =>
    e -> TransactionT md m ()
recordSingle event = undefined {- TransactionT $ do
    projector <- tcProjector <$> ask
    let updates = changesForEvent projector event
    newProjs <- forM updates $ \(Update projId f) -> do
        proj <- unTransactionT $ readProjection projId
        pure $ (AnyProjectionId projId, NewProjection projId $ f proj)
    let insertNewProj (pId, newProj) = M.insert pId newProj
        insertAllProjs m = foldr insertNewProj m newProjs
    modify $ \ts@TransactionState{..} -> ts {
        tsNewEvents = tsNewEvents S.>< S.singleton (AnyEvent event),
        tsNewProjections = insertAllProjs tsNewProjections
    }-}

record :: (
    Serialized e,
    Monad m
    ) =>
    [e] -> TransactionT md m ()
record events = forM_ events recordSingle

mkTransactionContext :: ProjReader m -> AnyProjector -> metadata -> TransactionContext metadata m
mkTransactionContext = TransactionContext


runTransactionT :: Monad m => TransactionT md m a -> TransactionContext md m -> m (a, TransactionResult)
runTransactionT (TransactionT tr) ctx = toResult <$> runTr
    where
        toResult (a, st, _) = (a, stateToResult st)
        runTr = runRWST tr ctx $ TransactionState {
            tsNewEvents       = S.empty,
            tsReadProjections = M.empty,
            tsNewProjections  = M.empty
        }

stateToResult :: TransactionState -> TransactionResult
stateToResult TransactionState{..} = TransactionResult {
        trNewEvents = tsNewEvents,
        trNewProjs  = M.foldrWithKey (\k v l -> v : l ) [] tsNewProjections,
        trReadProjs = M.foldrWithKey (\k v l -> (mkReadProj k v) : l ) [] tsReadProjections
    }
    where
        mkReadProj (AnyProjectionId projId) (ver, _) = ReadProjection projId ver
