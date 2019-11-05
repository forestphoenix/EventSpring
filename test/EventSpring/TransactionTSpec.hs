{-# LANGUAGE ScopedTypeVariables #-}
module EventSpring.TransactionTSpec where

import           Control.Monad (forM, forM_)
import           Data.Foldable (toList)
import           Data.List     (filter, nub, sort)
import           Data.Typeable (cast)

import           Common

spec :: Spec
spec = do
    describe "reading without writes" $ do
        it "A non-existant projection should not equal 'Nothing'" $ property $
            \(projId :: A) -> (Nothing ===) $ fst $ runTestTransaction testContextWithoutValues $ do
                readProjection projId
        it "A present projection has the stored value" $ property $
            \(projId :: A) -> (Just (B 2) ===) $ fst $ runTestTransaction testContextWithValues $ do
                readProjection projId
        it "Should read a stored projection only once, if a value is stored" $ property $
            \(projIds :: [A]) -> (length (nub projIds) ===) $ snd $
              countTestTransaction testContextWithValues $ do
                forM_ projIds readProjection
        it "Should read a stored projection only once, without a stored value" $ property $
            \(projIds :: [A]) -> (length (nub projIds) ===) $ snd $
              countTestTransaction testContextWithoutValues $ do
                forM_ projIds readProjection
        it "Should record the read" $ property $
            \(projIds :: [A]) ->
                let expectedWithVals = (\p -> ReadProjection (AnyProjId p) $ mkVersion 42) <$> (nub $ projIds)
                    expectedWithoutVals = (\p -> ReadProjection (AnyProjId p) versionZero) <$> (nub $ projIds)
                in
                conjoin [
                    unorderedEquals expectedWithVals $
                        trReadProjs $ snd $ runTestTransaction testContextWithValues $
                            forM_ projIds readProjection,
                    unorderedEquals expectedWithoutVals $
                        trReadProjs $ snd $ runTestTransaction testContextWithoutValues $
                            forM_ projIds readProjection
                    ]

    describe "writing events & reading them" $ do
        it "written events become part of the result" $ property $
            \(events :: [TestEvA]) -> (AnyEvent <$> events ===) $
              toList $ trNewEvents $ snd $ runTestTransaction testContextWithoutValues $ do
                forM_ events $ recordSingle
        it "projections change according to new events" $ property $
            \(events :: [TestEvB]) -> fst $ runTestTransaction testContextWithoutValues $ do
                record events
                let distinctEvents = nub events
                projProps <- forM distinctEvents $ \(TestEvB i) -> do
                    cnt <- readProjection $ B i
                    pure $ cnt === (Just . C . toInteger) (length $ filter (== (TestEvB i)) events)
                pure $ conjoin projProps
        it "new projections should be recorded" $ property $
            \(events :: [TestEvB]) ->
                let expectedNewProjs = expectedProj <$> nub events
                    expectedProj (TestEvB e) = NewProjection
                        (mkAnyProjId (B e))
                        (mkAnyProjection (C (toInteger (length (filter (== TestEvB e) events)))))
                    actualNewProjs = trNewProjs $ snd $ runTestTransaction testContextWithValues $
                        record events
                in
                counterexample (
                    "expected: " ++ show expectedNewProjs ++
                    "\nactual:   " ++ show actualNewProjs
                ) $ unorderedEquals expectedNewProjs actualNewProjs
