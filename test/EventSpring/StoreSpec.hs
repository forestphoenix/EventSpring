module EventSpring.StoreSpec where

import           Data.IORef

import           Common
import           EventSpring.Store

mkTestStore :: IO (Store, IORef [AnyEvent])
mkTestStore = do
    eventRef <- newIORef []
    let writeToEventRef events = atomicModifyIORef' eventRef (\old -> (events ++ old, ()))
    store <- mkEmptyStore writeToEventRef testProjector
    return (store, eventRef)

spec :: Spec
spec = do
    describe "basic operations" $ do
        it "reading an absent projection returns 'Nothing' with a version of zero" $ do
            (store, _) <- mkTestStore
            proj <- readStoredProjection store (A 0)
            proj `shouldBe` Nothing

        it "the results of a simple transation can be read" $ do
            (store, _) <- mkTestStore
            runTransaction store $ do
                recordSingle $ TestEvA 2
            proj <- readStoredProjection store (A 2)
            proj `shouldBe` Just (mkVersion 1, B 10)

    xdescribe "two concurrent calls to runTransaction" $ do
        it "write all events & projections as if they were executed in series" $ "todo" `shouldBe` "completed"

-- TODO: test for concurrency faults on the projection map
