{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
import           Debug.Trace                       (trace)
import           GHC.Generics                      (Generic)

import           Control.Category                  as ReExport hiding (id, (.))
import           Test.Hspec                        as ReExport
import           Test.QuickCheck                   as ReExport
import           Test.QuickCheck.Poly              as ReExport

import           EventSpring.Serialized            as ReExport
import           EventSpring.Common                as ReExport
import           EventSpring.Projection            as ReExport
import           EventSpring.TransactionT          as ReExport

isIdentity :: (Eq a, Show a, Arbitrary a) => (a -> a) -> Property
isIdentity f = property $ \a -> f a === a

isConst :: (Eq b, Show b, Show a, Arbitrary a) => b -> (a -> b) -> Property
isConst b f = property $ \a -> f a === b

traceId :: Show a => a -> a
traceId a = trace (show a) a


deriving instance Generic (Delta p)
instance ( Arbitrary p,
           Arbitrary (ProjectionFor p),
           Function (ProjectionFor p),
           CoArbitrary (ProjectionFor p)
         ) => Arbitrary (Delta p) where
    arbitrary = oneof [
        Create <$> arbitrary <*> arbitrary,
        Update <$> arbitrary <*> (applyFun <$> arbitrary),
        CreateOrUpdate <$> arbitrary <*> arbitrary <*> (applyFun <$> arbitrary)
        ]
    shrink = recursivelyShrink

instance Arbitrary AnyProjId where
    arbitrary = oneof [
        AnyProjId <$> (arbitrary :: Gen A),
        AnyProjId <$> (arbitrary :: Gen B)
        ]

-- Test Projections & Ids

deriving instance Generic A
deriving instance Hashable A
instance ToJSON A
instance FromJSON A
instance ProjId A
instance Projection A

deriving instance Generic B
deriving instance Hashable B
instance ToJSON B
instance FromJSON B
instance ProjId B
instance Projection B

deriving instance Generic C
deriving instance Hashable C
instance ToJSON C
instance FromJSON C
instance ProjId C
instance Projection C

type instance ProjectionFor A = B
type instance ProjectionFor B = C
type instance ProjectionFor C = [TestEvC]

-- Test Events


newtype TestEvA = TestEvA Integer
    deriving (Eq, Show, Arbitrary, Generic, Typeable, Hashable)
instance ToJSON TestEvA
instance FromJSON TestEvA
instance Event TestEvA

newtype TestEvB = TestEvB Integer
    deriving (Eq, Show, Arbitrary, Generic, Typeable, Hashable)
instance ToJSON TestEvB
instance FromJSON TestEvB
instance Event TestEvB

newtype TestEvC = TestEvC Integer
    deriving (Eq, Show, Arbitrary, Generic, Typeable, Hashable)
instance ToJSON TestEvC
instance FromJSON TestEvC
instance Event TestEvC
instance Projection [TestEvC]

instance (Eq a, Show a, ToJSON a, FromJSON a, Typeable a) => Serialized a where
    serialize = encode
    deserialize = eitherDecode

testProjector :: Projector AnyProjId
testProjector = Projector [
    toAnyOnEvent $ OnEvent $ \(TestEvA i) -> [
        Create (A i) (B 10)
        ],
    toAnyOnEvent $ OnEvent $ \(TestEvB i) -> [
        CreateOrUpdate (B i) (C 1) (\(C x) -> C $ x + 1)
        ],
    toAnyOnEvent $ OnEvent $ \(TestEvC i) -> [
        CreateOrUpdate (C 0) [TestEvC i] (\ls -> (TestEvC i):ls)
        ]
    ]

testContextWithoutValues :: Monad m => TransactionContext (WriterT (Sum Int) m)
testContextWithoutValues = mkTransactionContext
    (\projId -> do
        tell $ Sum 1
        pure Nothing
    )
    testProjector

testContextWithValues :: Monad m => TransactionContext (WriterT (Sum Int) m)
testContextWithValues = mkTransactionContext
    (\projId -> do
        tell $ Sum 1
        pure $ readValue $ typeOf projId
    )
    testProjector
        where
            readValue ty
                | ty == typeRep (Proxy :: Proxy A) = cast (mkVersion 42, B 2)
                | otherwise                               = Nothing

runTestTransaction ::
    TransactionContext (Writer (Sum Int)) ->
    TransactionT (Writer (Sum Int)) a -> (a, TransactionResult)
runTestTransaction ctx tr = fst $ runWriter $ runTransactionT tr ctx

countTestTransaction ::
    TransactionContext (Writer (Sum Int)) ->
    TransactionT (Writer (Sum Int)) a -> (a, Int)
countTestTransaction ctx tr = repack $ runWriter $ runTransactionT tr ctx
    where
        repack ((result, _), Sum rs) = (result, rs)

unorderedEquals :: (Show a, Eq a) => [a] -> [a] -> Property
unorderedEquals l1 l2 = property $
    length l1 === length l2 .&&.
    conjoin ((\i -> disjoin $ (=== i) <$> l1) <$> l2)
