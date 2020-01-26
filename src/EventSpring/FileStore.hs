module EventSpring.FileStore where

import qualified Conduit                       as C
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad                 (forM_)
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Conduit.Combinators      as C
import           System.IO                     (FilePath, IOMode (..),
                                                openBinaryFile)

import           EventSpring.Common
import           EventSpring.Internal.EventLog
import           EventSpring.Projection
import           EventSpring.Serialized
import           EventSpring.Store

newtype FileStore = FileStore Store

data WriteMsg event = WriteEvents [event] | FinishWrites

-- Warning: Since we are dealing with impure code here,
-- anything more complex than trivial should be refactored.

mkFileStore :: (SerializedType -> BS.ByteString -> Either String AnyEvent) -> FilePath -> Projector AnyProjId -> IO FileStore
mkFileStore lookupType fileName projector = do
    eventsToWrite <- newChan

    let eventsToChan events = writeChan eventsToWrite $ WriteEvents events
    store <- mkEmptyStore eventsToChan projector
    C.withSourceFile fileName $ \fileConduit -> C.runConduit $
        fileConduit C..|
        deserializeConduit lookupType C..|
        C.awaitForever (\event -> C.liftIO $ applyEventsRaw store [event]) -- this is horribly inefficient.

    writeThread <- async $ writeEvents fileName eventsToWrite

    pure $ FileStore store

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
