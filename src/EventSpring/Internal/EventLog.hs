{-# LANGUAGE RecordWildCards #-}

module EventSpring.Internal.EventLog where

import           Control.Monad.Catch               (MonadThrow)
import qualified Data.Binary                       as BN
import qualified Data.Binary.Get                   as BN
import qualified Data.Binary.Put                   as BN
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BSL
import           Data.Conduit                      as C
import           Data.Conduit.Combinators          as C
import qualified Data.Conduit.Serialization.Binary as CBN
import qualified Data.Digest.CRC32                 as CRC
import           Data.Word                         (Word32, Word64)

import           EventSpring.Serialized

{-
  File Format:
  (8-byte length, little endian)
  (n-byte type)  }
  (1-byte zero)  }} Handled by Serialized
  (n-byte event) }
  (4-byte crc of length and event, little endian)
-}

newtype SerializedEvent = SerializedEvent {
    seData :: BSL.ByteString
}

instance BN.Binary SerializedEvent where
    put (SerializedEvent{..}) = do
        let putFirstPart = do
                let length64 = (fromIntegral $ BSL.length seData) :: Word64
                BN.putWord64le length64
                BN.putLazyByteString seData
            firstPart = BN.runPut putFirstPart
            crc = CRC.crc32 firstPart
        BN.putLazyByteString firstPart
        BN.putWord32le crc

    get = do
        length64 <- BN.getWord64le
        dat <- BN.getLazyByteString $ fromIntegral length64
        fileCrc <- BN.getWord32le
        let actualCrc = CRC.crc32 $ BN.runPut (BN.putWord64le length64) <> dat
        pure $ SerializedEvent dat
        if fileCrc == actualCrc
            then pure $ SerializedEvent dat
            else fail $ "Crc mismatch in EventSpring.Internal.EventLog:get SerializedEvent read = " ++
                show fileCrc ++ " computed = " ++ show actualCrc

serializeConduit :: (Monad m, MonadThrow m) => ConduitT AnySerialized BS.ByteString m ()
serializeConduit = C.map (\dat -> SerializedEvent $ serializeAny dat) .| CBN.conduitEncode

deserializeConduit :: (Monad m, MonadThrow m) => (SerializedType -> BSL.ByteString -> Either String AnySerialized) -> ConduitT BS.ByteString AnySerialized m ()
deserializeConduit lookupType =
    C.filter (not . BS.null) .|
    CBN.conduitDecode .|
    C.map (\(SerializedEvent dat) -> deserializeAny dat) .|
    deserializeFully
        where
            deserializeFully = awaitForever $ \(PartialDeserialized typ dat) -> do
                case lookupType typ dat of
                    (Left err)  -> fail err
                    (Right dat) -> yield dat
