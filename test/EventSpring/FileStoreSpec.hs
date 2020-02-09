module EventSpring.FileStoreSpec where

import           Control.Concurrent.Async
import           Data.IORef
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (forM)

import           Common
import           EventSpring.FileStore
import           EventSpring.Store
import           EventSpring.TransactionT
import           EventSpring.TypeLookup

spec :: Spec
spec = do
    it "can read a projection after closing the file" $ property $
      \(events :: [TestEvB]) (projIds :: [B]) -> ioProperty $ withSystemTempDirectory "canreadtest" $ \dir -> do
        let file = (dir ++ "/store.eslog")

        originalStore <- mkFileStore (lookupType deserializeInfo) file testProjector
        originalProjections <- runFileTransaction originalStore $ do
            record events
            forM projIds $ \projId -> readProjection projId
        closeFileStore originalStore

        loadedStore <- mkFileStore (lookupType deserializeInfo) file testProjector
        loadedProjections <- runFileTransaction loadedStore $ do
            forM projIds $ \projId -> readProjection projId
        closeFileStore loadedStore

        pure $ originalProjections === loadedProjections

    xit "will have updated projections if the file is opened with a differen projector" $ do
        putStrLn $ "TODO"

deserializeInfo =
    toEventLookup (mkLookup :: TypeLookup TestEvB) <>
    toEventLookup (mkLookup :: TypeLookup TestEvA)
