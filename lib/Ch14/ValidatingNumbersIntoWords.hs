module Ch14.ValidatingNumbersIntoWords where

import Ch8.NumberIntoWords (digitToWord, digits, wordNumber)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns Zero for 0" $ digitToWord 0 `shouldBe` "Zero"
    it "returns One for 1" $ digitToWord 1 `shouldBe` "One"
  describe "digits" $ do
    it "returns [1] for 1" $ digits 1 `shouldBe` [1]
    it "returns [1,0,0] for 100" $ digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "One-Zero-Zero for 100" $ wordNumber 100 `shouldBe` "One-Zero-Zero"
    it "Nine-Zero-Zero-One for 9001" $ wordNumber 9001 `shouldBe` "Nine-Zero-Zero-One"