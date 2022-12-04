module UtilsSpec where
import Test.Hspec ( describe, it, shouldBe, Spec )
import Utils (rangeOverlaps)


spec :: Spec
spec = do
    describe "range overlap tests" $ do
        describe "works for increasing ranges" $ do
            it "when overlaps by more than 1 element" $ do
                rangeOverlaps (1, 10) (7, 11) `shouldBe` True
            it "when overlaps by 1 element" $ do 
                rangeOverlaps (1, 10) (10, 12) `shouldBe` True
            it "when does not overlap" $ do
                rangeOverlaps (1, 10) (11, 12) `shouldBe` False
        describe "works for decreasing ranges" $ do
            it "when overlaps by more than 1 element" $ do
                rangeOverlaps (10, 1) (11, 7) `shouldBe` True
            it "when overlaps by 1 element" $ do 
                rangeOverlaps (10, 11) (12, 10) `shouldBe` True
            it "when does not overlap" $ do
                rangeOverlaps (10, 1) (12, 11) `shouldBe` False