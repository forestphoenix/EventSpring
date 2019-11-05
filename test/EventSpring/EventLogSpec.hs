module EventSpring.EventLogSpec (spec) where

import           Common

import qualified Data.ByteString.Lazy          as BS
import           Data.Conduit                  as C
import           Data.Conduit.Combinators      as C
import           Data.Conduit.List             as CL

import           EventSpring.Internal.EventLog

spec :: Spec
spec = do
    describe "The Serialization" $ do
        it "will serialize an empty sequence to an empty file" $
            runSerialize [] `shouldBe` []

        it "can serialize a single events" $
            runSerialize [AnySerialized (TestEvA 42)] `shouldBe` [BS.pack
                [0,0,0,10 -- 4-Byte length
                ,84,101,115,116,69,118,65 -- type
                ,0  -- terminator for type
                ,52,50 -- data
                ,214,181,178,78] -- crc
                ]

        it "can serialize multiple events" $
            runSerialize [AnySerialized (TestEvA 42), AnySerialized (TestEvB 86)] `shouldBe`
              [ BS.pack
                  [0,0,0,10 -- 4-Byte length
                  ,84,101,115,116,69,118,65 -- type
                  ,0  -- terminator for type
                  ,52,50 -- data
                  ,214,181,178,78] -- crc
              , BS.pack
                  [0,0,0,10 -- 4-Byte length
                  ,84,101,115,116,69,118,66 -- type
                  ,0  -- terminator for type
                  ,56,54 -- data
                  ,111,216,150,181] -- crc
              ]

    describe "The Deserialization" $ do
        it "will deserialize an empty file to an empty sequence" $
            runDeserialize [] `shouldBe` []

        xit "can deserialize a single event" $ "this" `shouldBe` "TODO"
        xit "can deserialize multiple events" $ "this" `shouldBe` "TODO"
        xit "will detect an incorrect crc" $ "this" `shouldBe` "TODO"
    xit "the deserialization can read any serialized sequence" $ "this" `shouldBe` "TODO"

runSerialize list = runConduitPure $ CL.sourceList list .| serializeConduit .| C.sinkList

runDeserialize list = runConduitPure $ CL.sourceList list .| deserializeConduit undefined .| C.sinkList
