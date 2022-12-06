module RockPaperScissorsSpec where
import Test.Hspec ( describe, it, shouldBe, Spec )
import Utils
data TestEnum = Min | Middle | Max deriving (Eq, Enum, Bounded, Show)

spec :: Spec
spec = do
    describe "wrapSucc tests" $ do
        it "returns successor, wrapping around for maximum element" $ do
            wrapSucc Min `shouldBe` Middle
            wrapSucc Middle `shouldBe` Max
            wrapSucc Max `shouldBe` Min
    describe "moveForOutcome tests" $ do
        it "correctly calculates the move needed for win" $ do
            moveForOutcome Win Paper `shouldBe` Scissors
            moveForOutcome Win Scissors `shouldBe` Rock
            moveForOutcome Win Rock `shouldBe` Paper
        it "correctly calculates the move needed for loss" $ do
            moveForOutcome Loss Paper `shouldBe` Rock
            moveForOutcome Loss Scissors `shouldBe` Paper
            moveForOutcome Loss Rock `shouldBe` Scissors
        it "correctly calculates the move needed for draw" $ do
            moveForOutcome Draw Paper `shouldBe` Paper
            moveForOutcome Draw Scissors `shouldBe` Scissors
            moveForOutcome Draw Rock `shouldBe` Rock
        

            