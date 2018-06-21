{-# LANGUAGE ScopedTypeVariables #-}
module EventSpring.TransactionTSpec where

import           Control.Monad (forM_, forM)
import           Data.Foldable (toList)
import           Data.List     (sort, nub, filter)
import           Data.Typeable (cast)

import           Common

spec :: Spec
spec = do
    describe "reading without writes" $ do
        it "A new projection has its initial Value" $ property $
            \(projId :: TestId) -> (Just (TestProj 0 0) ===) $ fst $ runTestTransaction testContextWithoutValues $ do
                readProjection projId
        it "A present projection has the stored value" $ property $
            \(projId :: TestId) -> (Just (TestProj 2 2) ===) $ fst $ runTestTransaction testContextWithValues $ do
                readProjection projId
        it "Should read a stored projection only once, if a value is stored" $ property $
            \(projIds :: [TestId]) -> (length (nub projIds) ===) $ snd $
              countTestTransaction testContextWithValues $ do
                forM_ projIds readProjection
        it "Should read a stored projection only once, without a stored value" $ property $
            \(projIds :: [TestId]) -> (length (nub projIds) ===) $ snd $
              countTestTransaction testContextWithoutValues $ do
                forM_ projIds readProjection
        it "Should record the read" $ property $
            \(projIds :: [TestId]) ->
                let expectedWithVals = (\p -> ReadProjection p $ mkVersion 42) <$> (nub $ projIds)
                    expectedWithoutVals = (\p -> ReadProjection p versionZero) <$> (nub $ projIds)
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
            \(events :: [TestEvA]) -> fst $ runTestTransaction testContextWithoutValues $ do
                forM_ events $ recordSingle
                let distinctEvents = nub events
                projProps <- forM distinctEvents $ \(TestEvA i) -> do
                    (Just (TestProj cnt _)) <- readProjection $ TestId i
                    pure $ cnt === (length $ filter (== (TestEvA i)) events)
                pure $ conjoin projProps
        it "new projections should be recorded" $ property $
            \(events :: [TestEvA]) ->
                let expectedNewProjs = expectedProj <$> nub events
                    expectedProj (TestEvA e) = NewProjection
                        (TestId e)
                        (TestProj (2 + length (filter (== TestEvA e) events)) 2)
                    actualNewProjs = trNewProjs $ snd $ runTestTransaction testContextWithValues $
                        record events
                in
                counterexample (
                    "expected: " ++ show expectedNewProjs ++
                    "\nactual:   " ++ show actualNewProjs
                ) $ unorderedEquals expectedNewProjs actualNewProjs
