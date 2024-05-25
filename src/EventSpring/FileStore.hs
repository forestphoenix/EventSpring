module EventSpring.FileStore where

import qualified Conduit                       as C
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad                 (forM_, when)
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Conduit.Combinators      as C
import           System.IO                     (FilePath, IOMode (..),
                                                openBinaryFile)
import System.Directory (doesFileExist)

import           EventSpring.Common
import           EventSpring.Internal.EventLog
import           EventSpring.Projection
import           EventSpring.Serialized
import           EventSpring.Store
import           EventSpring.TransactionT
import           EventSpring.TypeLookup

import GHC.Stack

data FileStore = FileStore {
    fsStore       :: Store,
    fsWriteChan   :: Chan (WriteMsg AnyEvent),
    fsWriteThread :: Async ()
}

data WriteMsg event = WriteEvents [event] | FinishWrites

-- Warning: Since we are dealing with impure code here,
-- anything more complex than trivial should be refactored.

mkFileStore :: HasCallStack => (SerializedType -> BS.ByteString -> Either DeserializeFailed AnyEvent) -> FilePath -> Projector AnyProjId -> IO FileStore
mkFileStore lookupType fileName projector = do
    eventsToWrite <- newChan

    let eventsToChan events _ = writeChan eventsToWrite $ WriteEvents events
    store <- mkEmptyStore (WriteNewEvents eventsToChan) projector

    storeAlreadyPresent <- doesFileExist fileName
    when storeAlreadyPresent $ C.withSourceFile fileName $ \fileConduit -> C.runConduit $
        fileConduit C..|
        deserializeConduit lookupType C..|
        C.awaitForever (\event -> C.liftIO $ applyEventsRaw store [event]) -- this is horribly inefficient.

    writeThread <- async $ writeEvents fileName eventsToWrite

    pure $ FileStore store eventsToWrite writeThread

closeFileStore :: FileStore -> IO ()
closeFileStore store = do
    writeChan (fsWriteChan store) FinishWrites
    wait $ fsWriteThread store

runFileTransaction :: FileStore -> TransactionT IO a -> IO a
runFileTransaction fileStore = runTransaction (fsStore fileStore)

writeEvents :: FilePath -> Chan (WriteMsg AnyEvent) -> IO ()
writeEvents fileName eventsToWrite = C.runResourceT $ C.runConduit $
    nextEvent C..|
    serializeConduit C..|
    C.sinkIOHandle openEventLog
        where
            openEventLog = openBinaryFile fileName AppendMode
            nextEvent = do
                toWrite <- C.liftIO $ readChan eventsToWrite
                case toWrite of
                    FinishWrites -> pure ()
                    (WriteEvents events) -> do
                        forM_ events (\(AnyEvent event) -> C.yield $ AnySerialized event)
                        nextEvent
