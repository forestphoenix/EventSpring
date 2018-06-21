{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Common (
    module Common,
    module ReExport
) where

import           Control.Monad.Trans.Writer.Strict
import           Data.Aeson                        (FromJSON, ToJSON,
                                                    eitherDecode, encode)
import           Data.Functor.Identity             (Identity, runIdentity)
import           Data.Hashable                     (Hashable)
import           Data.Monoid
import           Data.Typeable                     (Proxy (..), Typeable, cast,
                                                    typeOf, typeRep)
import           EventSpring.Serialized
import           GHC.Generics                      (Generic)
import Debug.Trace (trace)

import           Test.Hspec                        as ReExport
import           Test.QuickCheck                   as ReExport
import           Control.Category                  as ReExport hiding ((.), id)

import           EventSpring.Common                as ReExport
import           EventSpring.Projection            as ReExport
import           EventSpring.TransactionT          as ReExport

isIdentity :: (Eq a, Show a, Arbitrary a) => (a -> a) -> Property
isIdentity f = property $ \a -> f a === a

isConst :: (Eq b, Show b, Show a, Arbitrary a) => b -> (a -> b) -> Property
isConst b f = property $ \a -> f a === b

traceId :: Show a => a -> a
traceId a = trace (show a) a

newtype TestId = TestId Int deriving (Eq, Show, Arbitrary, Generic, Typeable, Hashable)
instance ToJSON TestId
instance FromJSON TestId

data TestProj = TestProj Int Int deriving (Eq, Show, Generic, Typeable)
instance ToJSON TestProj
instance FromJSON TestProj

type instance ProjectionFor TestId = TestProj

newtype TestEvA = TestEvA Int
    deriving (Eq, Show, Arbitrary, Generic, Typeable)
instance ToJSON TestEvA
instance FromJSON TestEvA

newtype TestEvB = TestEvB Int
    deriving (Eq, Show, Arbitrary, Generic, Typeable)
instance ToJSON TestEvB
instance FromJSON TestEvB

instance (Eq a, ToJSON a, FromJSON a, Typeable a) => Serialized a where
    serialize = encode
    deserialize = eitherDecode

testProjector :: AnyProjector
testProjector = toAnyProjector $ Projector [
    OnEvent (\(TestEvA i) -> Create (TestId i) (TestProj 10 1))
    ]

testContextWithoutValues :: Monad m => TransactionContext () (WriterT (Sum Int) m)
testContextWithoutValues = mkTransactionContext
    (\projId -> do
        tell $ Sum 1
        pure Nothing
    )
    testProjector
    ()

testContextWithValues :: Monad m => TransactionContext () (WriterT (Sum Int) m)
testContextWithValues = mkTransactionContext
    (\projId -> do
        tell $ Sum 1
        pure $ readValue $ typeOf projId
    )
    testProjector
    ()
        where
            readValue ty
                | ty == (typeRep (Proxy :: Proxy TestId)) = cast $ (mkVersion 42, TestProj 2 2)
                | otherwise                               = Nothing

runTestTransaction ::
    TransactionContext md (Writer (Sum Int)) ->
    TransactionT md (Writer (Sum Int)) a -> (a, TransactionResult)
runTestTransaction ctx tr = fst $ runWriter $ runTransactionT tr ctx

countTestTransaction ::
    TransactionContext md (Writer (Sum Int)) ->
    TransactionT md (Writer (Sum Int)) a -> (a, Int)
countTestTransaction ctx tr = repack $ runWriter $ runTransactionT tr ctx
    where
        repack ((result, _), Sum rs) = (result, rs)

unorderedEquals :: (Show a, Eq a) => [a] -> [a] -> Property
unorderedEquals l1 l2 = property $
    length l1 == length l2 .&&.
    (conjoin $ (\i -> disjoin $ (=== i) <$> l1) <$> l2)
