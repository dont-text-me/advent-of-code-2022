module ListUtilsSpec where
import Test.Hspec ( describe, it, shouldBe, Spec, shouldSatisfy )
import Test.Hspec.QuickCheck (prop)
import Utils 
import Data.List (intersect)


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
    describe "range contains tests" $ do
        it "when contains" $ do
            rangeContains (1, 10) (7, 9) `shouldBe` True
        it "when does not overlap" $ do
            rangeContains (1, 10) (11, 12) `shouldBe` False
    describe "intersect3 tests" $ do
        it "works for empty lists" $ do
            intersect3 [] [] [] `shouldBe` ([] :: [Int])
        it "works for non-empty lists" $ do
            intersect3 [5, 6] [5] [1..10] `shouldBe` [5]
    describe "splitIntoGroups tests" $ do
        it "works for lists that can be split evenly" $ do
            splitIntoGroups 2 [1..10] `shouldSatisfy` (\x -> length x == 5 && all (\z -> length z == 2) x)