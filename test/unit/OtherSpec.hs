module OtherSpec where
import Test.Hspec


spec :: Spec
spec = do
    describe "sanity check" $ do
        it "check" $ do
            (1 + 1) `shouldBe` 2