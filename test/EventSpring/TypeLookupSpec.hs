module EventSpring.TypeLookupSpec where

import           Common
import           EventSpring.Projection
import           EventSpring.TypeLookup

import qualified Data.HashMap.Strict    as M

spec :: Spec
spec = do
    describe "lookupFromProjector" $ do
        it "finds a single type in a simple projector" $ do
            let projector = Projector [toAnyOnEvent $ OnEvent $ \(TestEvA i) -> noDeltas]
                (TypeLookup typeMap) = lookupFromProjector projector
            M.keys typeMap `shouldBe` [SerializedType "TestEvA"]

        it "finds multiple types in a more complex projector" $ do
            let projector = Projector [ toAnyOnEvent $ OnEvent $ \(TestEvA i) -> noDeltas
                                      , toAnyOnEvent $ OnEvent $ \(TestEvB i) -> noDeltas ]
                (TypeLookup typeMap) = lookupFromProjector projector
            M.keys typeMap `shouldBe` [SerializedType "TestEvA", SerializedType "TestEvB"]
