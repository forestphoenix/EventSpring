{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module EventSpring.Internal.TransactionT where

import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.RWS.Strict  (RWST, runRWST)
import qualified Data.HashMap.Strict             as M
import qualified Data.Sequence                   as S
import           Data.Typeable                   (cast)

import           EventSpring.Internal.Common
import           EventSpring.Internal.Projection
import           EventSpring.Serialized

type ProjReader m = forall projId proj.
    (projId `IsProjectionIdFor` proj, Serialized projId, Serialized proj) =>
    projId -> m (Maybe proj)

data NewProjection = forall projId proj. (projId `IsProjectionIdFor` proj) =>
    NewProjection projId proj

data ReadProjection = forall projId proj. (projId `IsProjectionIdFor` proj) =>
    ReadProjection projId ProjectionVersion

data TransactionContext projector md m = TransactionContext {
    tcReadProjection :: ProjReader m,

    tcProjector :: projector,

    tcMetadata :: md
}

data TransactionState = TransactionState {
    tsNewEvents :: S.Seq AnyEvent,
    tsReadProjections :: M.HashMap AnyProjectionId (ProjectionVersion, AnyProjection),
    tsNewProjections :: M.HashMap AnyProjectionId AnyProjection
}

data TransactionResult = TransactionResult {
    trNewEvents :: S.Seq AnyEvent,
    trNewProjs  :: [NewProjection],
    trReadProjs :: [ReadProjection]
}

newtype TransactionT p md m a = TransactionT (RWST (TransactionContext p md m) () TransactionState m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (TransactionT p md) where
    lift m = TransactionT $ lift m

readCachedProjection :: forall proj projId. Serialized proj =>
    TransactionState -> AnyProjectionId -> Maybe proj
readCachedProjection TransactionState{..} projId = castToProj =<< M.lookup projId tsReadProjections
    where
        castToProj (_, AnyProjection proj) = (cast proj) :: Maybe proj

readProjection :: (projId `IsProjectionIdFor` proj, Serialized projId, Serialized proj) =>
    projId -> TransactionT p md m proj
readProjection = undefined

mkTransactionContext :: ProjReader m -> projector -> metadata -> TransactionContext projector metadata m
mkTransactionContext = TransactionContext


runTransactionT :: Monad m => TransactionT p md m a -> TransactionContext p md m -> m (a, TransactionResult)
runTransactionT (TransactionT tr) ctx = toResult <$> runTr
    where
        toResult (a, st, _) = (a, stateToResult st)
        runTr = runRWST tr ctx $ TransactionState {
            tsNewEvents = S.empty,
            tsReadProjections = M.empty,
            tsNewProjections = M.empty
        }

stateToResult :: TransactionState -> TransactionResult
stateToResult TransactionState{..} = TransactionResult {
        trNewEvents = tsNewEvents,
        trNewProjs  = M.foldrWithKey (\k v l -> (mkNewProj k v) : l ) [] tsNewProjections,
        trReadProjs = M.foldrWithKey (\k v l -> (mkReadProj k v) : l ) [] tsReadProjections
    }
    where
        mkNewProj = undefined
        mkReadProj = undefined
