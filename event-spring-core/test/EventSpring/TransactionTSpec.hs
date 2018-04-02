module EventSpring.TransactionTSpec where

import Common

spec :: Spec
spec = do
    describe "reading without writes" $ do
        it "A new projection has its initial Value" $ property $
            \(projId :: TestId) -> (TestProj 0 0 ===) $ fst $ runTestTransaction testContextWithoutValues $ do
                readProjection projId
        it "A present projection has the stored value" $ property $
            \(projId :: TestId) -> (TestProj 2 2 ===) $ fst $ runTestTransaction testContextWithValues $ do
                readProjection projId
        xit "Should read a stored projection only once" False
    describe "writing events & reading them" $ do
        xit "TODO" False
