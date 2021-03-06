{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module EventSpring.ProjectionSpec (spec) where

import           Data.Function            (on)
import           Data.List                (deleteBy, lookup, nubBy)
import           Data.Maybe               (maybe)
import           Test.QuickCheck.Function (Fun, applyFun)
import           Test.QuickCheck.Poly

import           Common

instance Event A
instance Event B

spec :: Spec
spec = do
    describe "applyDeltaAL" $ do
        let areEqualExceptForId al1 al2 projId =
                deleteProj projId al1 === deleteProj projId al2
            deleteProj p = filter ((/=) p . fst)

            nubAL = nubBy ((==) `on` fst)

        it "Create inserts a new projected value" $ property $
            \(rawOldAL :: [(A, B)]) (newId :: A) (newVal :: B) ->
            let oldAL = nubAL rawOldAL
                newAL = applyDeltaAL (Create newId newVal) oldAL
            in
                areEqualExceptForId oldAL newAL newId .&&.
                lookup newId newAL === Just newVal

        it "Update updates an existing value" $ property $
            \(rawOldAL :: [(A, B)]) (pId :: A) (fn :: Fun B B) ->
            let oldAL = nubAL rawOldAL

                newValue = (applyFun fn) <$> lookup pId oldAL
                newAL = applyDeltaAL (Update pId (applyFun fn)) oldAL
            in
                areEqualExceptForId oldAL newAL pId .&&.
                lookup pId newAL === newValue

        it "CreateOrUpdate updates an existing value or creates a new one" $ property $
            \(rawOldAL :: [(A, B)]) (pId :: A) (newVal :: B) (fn :: Fun B B) ->
            let oldAL = nubAL rawOldAL

                newValue = maybe newVal (applyFun fn) $ lookup pId oldAL
                newAL = applyDeltaAL (CreateOrUpdate pId newVal (applyFun fn)) oldAL
            in
                areEqualExceptForId oldAL newAL pId .&&.
                lookup pId newAL === (Just newValue)
    describe "deltasForEvent" $ do
        it "Finds the Correct Deltas by type" $ property $
            \(i :: Integer) ->
            let projector = Projector [
                        OnEvent (\(B x) -> []),
                        OnEvent (\(A x) -> [Create (A x) (B x)])
                    ]
                unDelta (Create _ x) = x
            in
                [B i] === (unDelta <$> deltasForEvent projector (A i))
