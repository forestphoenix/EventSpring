{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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

import           Test.Hspec                        as ReExport
import           Test.QuickCheck                   as ReExport

import           EventSpring.Common                as ReExport
import           EventSpring.Projection            as ReExport
import           EventSpring.TransactionT          as ReExport

newtype TestId = TestId Int deriving (Eq, Show, Arbitrary, Generic, Typeable, Hashable)
instance ToJSON TestId
instance FromJSON TestId

data TestProj = TestProj Int Int deriving (Eq, Show, Generic, Typeable)
instance ToJSON TestProj
instance FromJSON TestProj

instance TestId `IsProjectionIdFor` TestProj

newtype TestEvA = TestEvA Int
    deriving (Eq, Show, Arbitrary, Generic, Typeable)
instance ToJSON TestEvA
instance FromJSON TestEvA

newtype TestEvB = TestEvB Int
    deriving (Eq, Show, Arbitrary, Generic, Typeable)
instance ToJSON TestEvB
instance FromJSON TestEvB


data TestProjector = TestProjector

instance CanHandleProjection TestProjector TestProj where
    initialProjection _ = TestProj 0 0

instance CanHandleEvent TestProjector TestEvA where
    changesForEvent _ (TestEvA e) = [Update (TestId e) update]
        where
            update (TestProj a b) = TestProj (a + 1) b

instance CanHandleEvent TestProjector TestEvB where
    changesForEvent _ (TestEvB e) = [Update (TestId e) update]
        where
            update (TestProj a b) = TestProj a (b + 1)

instance (Eq a, ToJSON a, FromJSON a, Typeable a) => Serialized a where
    serializer = Serializer encode eitherDecode

testContextWithoutValues :: Monad m => TransactionContext TestProjector () (WriterT (Sum Int) m)
testContextWithoutValues = mkTransactionContext
    (\projId -> do
        tell $ Sum 1
        pure Nothing
    )
    TestProjector
    ()

testContextWithValues :: Monad m => TransactionContext TestProjector () (WriterT (Sum Int) m)
testContextWithValues = mkTransactionContext
    (\projId -> do
        tell $ Sum 1
        pure $ readValue $ typeOf projId
    )
    TestProjector
    ()
        where
            readValue ty
                | ty == (typeRep (Proxy :: Proxy TestId)) = cast $ (mkVersion 42, TestProj 2 2)
                | otherwise                               = Nothing

runTestTransaction ::
    TransactionContext p md (Writer (Sum Int)) ->
    TransactionT p md (Writer (Sum Int)) a -> (a, TransactionResult)
runTestTransaction ctx tr = fst $ runWriter $ runTransactionT tr ctx

countTestTransaction ::
    TransactionContext p md (Writer (Sum Int)) ->
    TransactionT p md (Writer (Sum Int)) a -> (a, Int)
countTestTransaction ctx tr = repack $ runWriter $ runTransactionT tr ctx
    where
        repack ((result, _), Sum rs) = (result, rs)
