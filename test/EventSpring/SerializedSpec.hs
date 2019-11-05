{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EventSpring.SerializedSpec where

import           Data.Either            (fromLeft, fromRight)
import           EventSpring.Serialized

import           Common

fromExtractResult (ExtractOk a)                  = a
fromExtractResult (ExtractError err)             = error err
fromExtractResult (ExtractTypeMismatch mismatch) = error $ show mismatch

spec :: Spec
spec = do
    it "serializing and deserializing is the identity" $ isIdentity $
        AnySerialized @TestEvA >>>
        serializeAny >>>
        deserializeAny >>>
        extractPartial >>>
        fromExtractResult >>>
        id @TestEvA

    it "serializing and deserializing to a different type fails" $
        isConst (ExtractTypeMismatch (TypeMismatch "TestEvB" "TestEvA")) $
            AnySerialized @TestEvA >>>
            serializeAny >>>
            deserializeAny >>>
            extractPartial >>>
            id @(ExtractResult TestEvB)
