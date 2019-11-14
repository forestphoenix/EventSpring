{-# LANGUAGE FlexibleContexts #-}
module EventSpring.Store where

import           Control.Concurrent.MVar
import           Control.Monad.Logger
import qualified Data.HashMap.Strict      as M
import qualified Data.Sequence            as S

import           EventSpring.Common
import           EventSpring.Projection
import           EventSpring.Serialized
import           EventSpring.TransactionT

data Store = Store {
  stProjections :: MVar (M.HashMap AnyProjId (MVar (ProjectionVersion, AnyProjection))),
  stWriteEvents :: [AnyEvent] -> IO (),
  stProjector :: Projector AnyProjId
}

mkEmptyStore :: ([AnyEvent] -> IO ()) -> Projector AnyProjId -> IO Store
mkEmptyStore writeEvents projector = do
    projections <- newMVar M.empty
    pure $ Store projections writeEvents projector

tryCommitResults :: Store -> TransactionResult -> IO Bool
tryCommitResults = undefined

readStoredProjection :: (ProjId projId, Projection (ProjectionFor projId)) => Store -> projId -> IO (Maybe (ProjectionVersion, ProjectionFor projId))
readStoredProjection store projId = do
    projections <- readMVar $ stProjections store
    let projMVar = M.lookup (AnyProjId projId) projections
    case projMVar of
        Nothing     -> pure Nothing
        (Just mvar) -> convertSecond <$> readMVar mvar
        where
            convertSecond (a, b) = (,) a <$> convert b
            convert (AnyProjection serialized) = castAny serialized

runTransaction :: Store -> TransactionT IO a -> IO a
runTransaction store transaction = undefined

storeContext :: Store -> TransactionContext IO
storeContext store = mkTransactionContext (readStoredProjection store) (stProjector store)
