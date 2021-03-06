{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module EventSpring.EventLogSpec (spec) where

import           Common

import qualified Data.ByteString               as BS
import           Data.Conduit                  as C
import           Data.Conduit.Combinators      as C
import           Data.Conduit.List             as CL
import qualified Data.List                     as L (length, take)

import           EventSpring.Internal.EventLog

import EventSpring.TypeLookup

spec :: Spec
spec = do
    let eventA = TestEvA 42
        serializedA =
            [10,0,0,0,0,0,0,0 -- 8-Byte length
            ,84,101,115,116,69,118,65 -- type
            ,0  -- terminator for type
            ,52,50 -- data
            ,207,153,63,144] -- crc
        eventB = TestEvB 86
        serializedB =
            [10,0,0,0,0,0,0,0 -- 8-Byte length
            ,84,101,115,116,69,118,66 -- type
            ,0  -- terminator for type
            ,56,54 -- data
            ,52,189,82,41] -- crc
    describe "The Serialization" $ do
        it "will serialize an empty sequence to an empty file" $
            runSerialize [] `shouldBe` [[]]

        it "can serialize a single events" $ do
            serialized <- runSerialize [AnySerialized eventA]
            serialized `shouldBe` [BS.pack serializedA]

        it "can serialize multiple events" $ do
            serialized <- runSerialize [AnySerialized eventA, AnySerialized eventB]
            serialized `shouldBe`
              [BS.pack serializedA, BS.pack serializedB]

    describe "The Deserialization" $ do
        it "will deserialize an empty file to an empty sequence" $ do
            deserialized <- runDeserialize [BS.pack []]
            deserialized `shouldBe` []

        it "can deserialize a single event" $ do
            deserialized <- runDeserialize [BS.pack serializedA]
            deserialized `shouldBe` [AnyEvent eventA]

        it "can deserialize multiple events" $ do
            deserialized <- runDeserialize [BS.pack serializedA, BS.pack serializedB]
            deserialized `shouldBe` [AnyEvent eventA, AnyEvent eventB]

        it "can deserialize multiple events concatenated together" $ do
            deserialized <- runDeserialize [BS.pack serializedA <> BS.pack serializedB]
            deserialized `shouldBe` [AnyEvent eventA, AnyEvent eventB]

        it "will detect an incorrect length" $ do
            runDeserialize [BS.take 10 $ BS.pack serializedA] `shouldThrow` anyException

        it "will detect an incorrect crc" $ do
            let wrongCrc = L.take (L.length serializedA - 4) serializedA <> [1, 2, 3, 4]
            runDeserialize [BS.pack wrongCrc] `shouldThrow` anyException

    it "the deserialization can read any serialized sequence" $ property $
        \(events :: [Either TestEvA TestEvB]) -> ioProperty $ do
            let toAny (Left evA) = AnySerialized evA
                toAny (Right evB) = AnySerialized evB
                anyEvents = toAny <$> events
            serialized <- runSerialize anyEvents
            deserialized <- runDeserialize [mconcat serialized]
            let demotedDeserialized = (\(AnyEvent e) -> AnySerialized e) <$> deserialized
            return $ counterexample ("Serialized: " ++ show serialized) $ anyEvents === demotedDeserialized


runSerialize list = runConduit $ CL.sourceList list .| serializeConduit .| C.sinkList

deserializeInfo =
    toEventLookup (mkLookup :: TypeLookup TestEvB) <>
    toEventLookup (mkLookup :: TypeLookup TestEvA)

runDeserialize list = runConduit $
    CL.sourceList list .|
    deserializeConduit (lookupType deserializeInfo) .|
    C.sinkList
