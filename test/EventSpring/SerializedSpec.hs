{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module EventSpring.SerializedSpec where

import Data.Either (fromRight, fromLeft)
import EventSpring.Serialized

import           Common

spec :: Spec
spec = do
    it "serializing and deserializing is the identity" $ isIdentity $
        AnySerialized @TestEvA >>>
        serializeAny >>>
        deserializeAny >>>
        extractPartial >>>
        either (\msg -> error $ "deserialising should be successful, was " ++ msg) id >>>
        id @TestEvA

    it "serializing and deserializing to a different type fails" $
      isConst (Left "Error in extractPartial: type mismatch (expected TestEvB, got TestEvA)") $
        AnySerialized @TestEvA >>>
        serializeAny >>>
        deserializeAny >>>
        extractPartial >>>
        id @(Either String TestEvB)
