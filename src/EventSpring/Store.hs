{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
module EventSpring.Store where

import           Control.Concurrent.MVar
import           Control.Monad            (forM, forM_, unless)
import           Control.Monad.Logger
import           Data.Either              (partitionEithers)
import qualified Data.HashMap.Strict      as M
import qualified Data.HashSet             as H
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
tryCommitResults (Store {..}) result@(TransactionResult {..}) = do
    let newProjsMap = newProjsToMap trNewProjs
    projMap <- readMVar stProjections

    (projsToCreate, existingProjections) <- acquireProjections projMap $ M.keys newProjsMap

    -- TODO: check versions
    let conflictOnExisting = False

    if conflictOnExisting
        then do
            releaseAndWriteProjections existingProjections
            pure False
        else do
            newProjections <- createProjections stProjections projsToCreate
            case newProjections of
                (Just newProjs) -> do
                    let projections = (snd <$> existingProjections) <> newProjs
                        newValuesMap = transformValues (fst <$> existingProjections) newProjsMap
                        toWrite = toWritableProjections newValuesMap projections
                    releaseAndWriteProjections toWrite
                    pure True
                Nothing -> do
                    releaseAndWriteProjections existingProjections
                    pure False

    -- phase 1: lock known projections (fail on conflict), check versions
    -- phase 2: atomically create new projections and lock them (or fail, if they already exist)
    -- phase 3: write new values (into both new and existing projections)
    -- phase 4: unlock the projections

toWritableProjections ::
    M.HashMap AnyProjId dat ->
    M.HashMap AnyProjId (MVar dat) ->
    M.HashMap AnyProjId (dat, MVar dat)
toWritableProjections dataMap mvarMap = M.mapWithKey mergeMVar dataMap
    where
        mergeMVar projId dat = (dat, forceMaybe $ M.lookup projId mvarMap)
        forceMaybe (Just a) = a

transformValues ::
    M.HashMap AnyProjId (ProjectionVersion, AnyProjection) ->
    M.HashMap AnyProjId AnyProjection ->
    M.HashMap AnyProjId (ProjectionVersion, AnyProjection)
transformValues existingValues newValues = M.mapWithKey updateExisting newValues
    where
        updateExisting projId newVal = (newVersion projId, newVal)
        newVersion projId = case M.lookup projId existingValues of
            (Just (version, _)) -> incrementVersion version
            Nothing             -> incrementVersion versionZero

releaseAndWriteProjections ::
    M.HashMap AnyProjId ((ProjectionVersion, AnyProjection), MVar (ProjectionVersion, AnyProjection)) ->
    IO ()
releaseAndWriteProjections toRelease = forM_ toRelease $ \(value, mvar) -> do
    putMVar mvar value

createProjections projMap projIds = modifyMVar projMap $ \currentProjs -> do
    let keysSet = H.fromList $ fst <$> M.toList currentProjs
        intersection = projIds `H.intersection` keysSet
        hasConflict = not $ H.null intersection
    if hasConflict
        then pure (currentProjs, Nothing)
        else do
            newMVars <- fmap M.fromList $ forM (H.toList projIds) $ \projId -> do
                putStrLn $ "create mvar for " ++ show projId
                mvar <- newEmptyMVar
                pure (projId, mvar)
            pure (currentProjs <> newMVars, Just newMVars)

acquireProjections ::
    M.HashMap AnyProjId (MVar (ProjectionVersion, AnyProjection)) ->
    [AnyProjId] ->
    IO (H.HashSet AnyProjId, M.HashMap AnyProjId ((ProjectionVersion, AnyProjection), MVar (ProjectionVersion, AnyProjection)))
acquireProjections projMap newProjs = do
    foundValues <- forM foundProjs $ \(projId, mvar) -> do
        value <- takeMVar mvar
        pure (projId, (value, mvar))
    pure (H.fromList notFound, M.fromList foundValues)
    where
        (foundProjs, notFound) = partitionEithers $ lookupProjId <$> newProjs
        lookupProjId projId = case M.lookup projId projMap of
            Nothing     -> Right projId
            (Just mvar) -> Left (projId, mvar)

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
runTransaction store transaction = do
    (out, result) <- runTransactionT transaction $ storeContext store
    ok <- tryCommitResults store result
    if ok
        then pure out
        else runTransaction store transaction

storeContext :: Store -> TransactionContext IO
storeContext store = mkTransactionContext (readStoredProjection store) (stProjector store)

newProjsToMap :: [NewProjection] -> M.HashMap AnyProjId AnyProjection
newProjsToMap projs = M.fromList $ toAny <$> projs
    where
        toAny (NewProjection projId projVal) = (projId, projVal)
