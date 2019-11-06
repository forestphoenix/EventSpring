module EventSpring.Internal.EventLog where

import qualified Data.Binary              as BN
import qualified Data.ByteString.Lazy     as BS
import           Data.Conduit             as C
import           Data.Conduit.Combinators as C
import qualified Data.Digest.CRC32        as CRC
import           Data.Word                (Word32)

import           EventSpring.Serialized

{-
  File Format:
  (4-byte length, big endian)
  (n-byte type)  }
  (1-byte zero)  }} Handled by Serialized
  (n-byte event) }
  (4-byte crc of length and event, big endian)
-}

serializeConduit :: Monad m => ConduitT AnySerialized BS.ByteString m ()
serializeConduit = awaitForever $ \input -> do
    let serialized = serializeAny input
        length32 :: Word32
        length32 = fromIntegral $ BS.length serialized
        lengthBS = BN.encode length32
        crc = CRC.crc32 lengthBS `CRC.crc32Update` serialized
        packet = lengthBS <> serialized <> BN.encode crc
    yield packet

deserializeConduit :: (SerializedType -> BS.ByteString -> Either String AnySerialized) -> ConduitT BS.ByteString AnySerialized m ()
deserializeConduit lookupType = do
    return ()

separateChunks :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
separateChunks = do
    rawLength <- C.take 8
    let length = computeLength rawLength
    undefined
        where
            computeLength bs = undefined
