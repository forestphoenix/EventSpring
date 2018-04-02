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

import           Data.Aeson               (FromJSON, ToJSON, eitherDecode,
                                           encode)
import           Data.Functor.Identity    (Identity, runIdentity)
import           Data.Hashable            (Hashable)
import           Data.Typeable            (Proxy (..), Typeable, cast, typeOf,
                                           typeRep)
import           EventSpring.Serialized
import           GHC.Generics             (Generic)

import           Test.Hspec               as ReExport
import           Test.QuickCheck          as ReExport

import           EventSpring.Projection   as ReExport
import           EventSpring.TransactionT as ReExport

newtype TestId = TestId Int deriving (Eq, Show, Arbitrary, Generic, Typeable, Hashable)
instance ToJSON TestId
instance FromJSON TestId

data TestProj = TestProj Int Int deriving (Eq, Show, Generic, Typeable)
instance ToJSON TestProj
instance FromJSON TestProj

instance TestId `IsProjectionIdFor` TestProj

data TestEvA = TestEvA Int
data TestEvB = TestEvB Int


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

testContextWithoutValues :: Applicative m => TransactionContext TestProjector () m
testContextWithoutValues = mkTransactionContext
    (\projId -> pure Nothing)
    TestProjector
    ()

testContextWithValues :: Applicative m => TransactionContext TestProjector () m
testContextWithValues = mkTransactionContext
    (\projId -> pure $ readValue $ typeOf projId)
    TestProjector
    ()
        where
            readValue ty
                | ty == (typeRep (Proxy :: Proxy TestId)) = cast $ (versionZero, TestProj 2 2)
                | otherwise                               = Nothing

runTestTransaction :: TransactionContext p md Identity -> TransactionT p md Identity a -> (a, TransactionResult)
runTestTransaction ctx tr = runIdentity $ runTransactionT tr ctx
