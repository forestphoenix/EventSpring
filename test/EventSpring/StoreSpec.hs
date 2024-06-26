module EventSpring.StoreSpec where

import           Data.IORef
import Control.Concurrent.Async

import           Common
import           EventSpring.Store
import Control.Monad (forM_)

mkTestStore :: IO (Store, IORef [AnyEvent])
mkTestStore = do
    eventRef <- newIORef []
    let writeToEventRef events _ = atomicModifyIORef' eventRef (\old -> (events ++ old, ()))
    store <- mkEmptyStore (WriteNewEvents writeToEventRef) testProjector
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

        it "the events written in a simple transation are written to the store" $ do
            (store, events) <- mkTestStore
            runTransaction store $ do
                recordSingle $ TestEvA 2
                record [TestEvB 1, TestEvB 2]
            writtenEvents <- readIORef events
            writtenEvents `shouldBe` [AnyEvent $ TestEvA 2, AnyEvent $ TestEvB 1, AnyEvent $ TestEvB 2]

    describe "concurrent calls to runTransaction" $ do
        it "two calls write projections as if they were executed in series" $ property $
          \(eventsA :: [TestEvC]) (eventsB :: [TestEvC]) -> ioProperty $ do
            (store, _) <- mkTestStore
            writeA <- async $ runTransaction store $ record eventsA
            writeB <- async $ runTransaction store $ record eventsB
            wait writeA
            wait writeB
            results <- readStoredProjection store (C 0)
            let resultProjection = (reverse . snd) <$> results
                resultVersion = fst <$> results
                nonEmptyEventLists = length $ filter (not . null) [eventsA, eventsB]
                noEventsWritten = nonEmptyEventLists == 0
            pure $
                ((resultProjection === Just (eventsA <> eventsB) .||.
                resultProjection === Just (eventsB <> eventsA)) .&&.
                (resultVersion === Just (mkVersion nonEmptyEventLists))) .||.
                (noEventsWritten .&&. results === Nothing)

        it "two calls write events as if they were executed in series" $ property $
          \(eventsA :: [TestEvC]) (eventsB :: [TestEvC]) -> ioProperty $ do
            (store, events) <- mkTestStore
            writeA <- async $ runTransaction store $ record eventsA
            writeB <- async $ runTransaction store $ record eventsB
            wait writeA
            wait writeB
            writtenEvents <- readIORef events
            pure $
                (writtenEvents === fmap AnyEvent (eventsA <> eventsB) .||.
                writtenEvents === fmap AnyEvent (eventsB <> eventsA))

    describe "applyEventRaw" $ do
        it "modfies the projections like (runTransation store  . record)" $ property $
          \(events :: [TestEvC]) -> ioProperty $ do
              (storeRaw, _) <- mkTestStore
              (storeRecord, _) <- mkTestStore

              applyEventsRaw storeRaw (AnyEvent <$> events)
              (runTransaction storeRecord . record) events

              recordedEventsRaw <- runTransaction storeRaw $ readProjection (C 0)
              recordedEventsRecord <- runTransaction storeRecord $ readProjection (C 0)

              pure $ recordedEventsRaw === recordedEventsRecord

        it "does not write any events" $ property $
          \(events :: [TestEvC]) -> ioProperty $ do
              (store, eventRef) <- mkTestStore

              applyEventsRaw store (AnyEvent <$> events)

              recordedEvents <- readIORef eventRef
              pure $ [] === recordedEvents
