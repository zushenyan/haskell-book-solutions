module Ch14.ShortExercise where

import Ch8.Recursion (myMul)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "MyMul" $ do
    it "myMul 0 1 should be 0" $ myMul 0 1 `shouldBe` 0
    it "myMul 1 0 should be 1" $ myMul 1 0 `shouldBe` 0
    it "myMul 1 1 should be 1" $ myMul 1 1 `shouldBe` 1
    it "myMul 1 2 should be 2" $ myMul 1 2 `shouldBe` 2
    it "myMul 2 1 should be 2" $ myMul 2 1 `shouldBe` 2
    it "myMul 2 2 should be 2" $ myMul 2 2 `shouldBe` 4