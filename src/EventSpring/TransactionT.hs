{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE Strict                     #-}
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
import qualified Data.Typeable                   as Typeable (cast, typeOf)

import           EventSpring.Common
import           EventSpring.Projection
import           EventSpring.Serialized

type ProjReader m = forall projId.
    (Serialized projId, Serialized (ProjectionFor projId)) =>
    projId -> m (Maybe (ProjectionVersion, ProjectionFor projId))

data NewProjection = NewProjection AnyProjId AnyProjection
    deriving (Eq, Show)

data ReadProjection = ReadProjection AnyProjId ProjectionVersion
    deriving (Eq, Show)

data TransactionContext md m = TransactionContext {
    tcReadProjection :: ProjReader m,

    tcProjector      :: Projector AnyProjId,

    tcMetadata       :: md
}

data TransactionState = TransactionState {
    tsNewEvents       :: S.Seq AnyEvent,
    tsReadProjections :: M.HashMap AnyProjId (ProjectionVersion, Maybe AnyProjection),
    tsNewProjections  :: M.HashMap AnyProjId NewProjection
}

data TransactionResult = TransactionResult {
    trNewEvents :: S.Seq AnyEvent,
    trNewProjs  :: [NewProjection],
    trReadProjs :: [ReadProjection]
}

newtype TransactionT md m a = TransactionT
    { unTransactionT :: RWST (TransactionContext md m) () TransactionState m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (TransactionT md) where
    lift m = TransactionT $ lift m

readCachedProjection :: forall proj projId. Serialized proj =>
    TransactionState -> AnyProjId -> Maybe (Maybe proj)
readCachedProjection TransactionState{..} projId = 
    fmap Just (castNewProj =<< M.lookup projId tsNewProjections) <|>
    (collapseLookupAndCast . (fmap castToProj . snd) =<< M.lookup projId tsReadProjections)
        where
            castToProj (AnyProjection proj) = (castAny proj) :: Maybe proj
            castNewProj (NewProjection _ proj) = castToProj proj

            collapseLookupAndCast (Just Nothing) = Nothing
            collapseLookupAndCast Nothing        = Just Nothing
            collapseLookupAndCast (Just a)       = Just a

loadProjection :: (
    Serialized projId, Serialized (ProjectionFor projId),
    Monad m) =>
    projId -> TransactionT md m (Maybe (ProjectionVersion, ProjectionFor projId))
loadProjection projId = TransactionT $ do
    reader <- tcReadProjection <$> ask
    loaded <- lift $ reader projId
    pure loaded

cacheProjection :: (
    Serialized projId, Hashable projId, Serialized (ProjectionFor projId),
    Monad m) =>
    projId -> ProjectionVersion -> Maybe (ProjectionFor projId) -> TransactionT md m ()
cacheProjection projId ver val = TransactionT $
    modify $ \state@TransactionState{..} -> state {
        tsReadProjections = M.insert (mkAnyProjId projId) (ver, mkAnyProjection <$> val) tsReadProjections
    }

readProjection :: (
    Serialized projId, Hashable projId, Serialized (ProjectionFor projId),
    Monad m) =>
    projId -> TransactionT md m (Maybe (ProjectionFor projId))
readProjection projId = TransactionT $ do
    trState <- get
    projector <- tcProjector <$> ask
    let cached = readCachedProjection trState $ mkAnyProjId projId
    case cached of
        Just mp  -> pure mp
        Nothing -> do
             loaded <- unTransactionT $ loadProjection projId
             case loaded of
                 Nothing -> do
                     unTransactionT $ cacheProjection projId versionZero Nothing
                     pure Nothing
                 Just (readVer, readProj) -> do
                     unTransactionT $ cacheProjection projId readVer $ Just readProj
                     return $ Just readProj

recordSingle :: (
    Serialized e,
    Monad m
    ) =>
    e -> TransactionT md m ()
recordSingle event =  TransactionT $ do
    projector <- tcProjector <$> ask
    let deltas = deltasForEvent projector event
    newProjs <- forM deltas $ \delta -> 
        let anyProjId = idFromDelta delta
        in
            onAnyProjId anyProjId $ \projId -> do
                mproj <- unTransactionT $ readProjection projId
                let mproj' = applyDelta delta ((anyProjId, ) . mkAnyProjection <$> mproj)

                pure $ ((anyProjId, ) . NewProjection anyProjId) . snd <$> mproj'

    let insertNewProj (Just (pId, newProj)) = M.insert pId newProj
        insertNewProj Nothing               = id
        insertAllProjs m = foldr insertNewProj m newProjs
    modify $ \ts@TransactionState{..} -> ts {
        tsNewEvents = tsNewEvents S.>< S.singleton (AnyEvent event),
        tsNewProjections = insertAllProjs tsNewProjections
    }

record :: (
    Serialized e,
    Monad m
    ) =>
    [e] -> TransactionT md m ()
record events = forM_ events recordSingle

mkTransactionContext :: ProjReader m -> Projector AnyProjId -> metadata -> TransactionContext metadata m
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
        mkReadProj projId (ver, _) = ReadProjection projId ver

