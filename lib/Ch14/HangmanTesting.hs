module Ch14.HangmanTesting where

import Ch13.Hangman (Puzzle (Puzzle), fillInCharacter, handleGuess)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  let wordToGuess = "foobar"
      discovered = replicate (length wordToGuess) Nothing
      puzzle = Puzzle wordToGuess discovered "cba"
  describe "fillInCharacter" $ do
    it "should update guessed list" $ do
      fillInCharacter puzzle 'd' `shouldBe` Puzzle wordToGuess discovered "dcba"
    it "should update discovered" $ do
      let newDiscovered1 = [Nothing, Just 'o', Just 'o', Nothing, Nothing, Nothing]
          newDiscovered2 = [Just 'f', Nothing, Nothing, Nothing, Nothing, Nothing]
      fillInCharacter puzzle 'o' `shouldBe` Puzzle wordToGuess newDiscovered1 "ocba"
      fillInCharacter puzzle 'f' `shouldBe` Puzzle wordToGuess newDiscovered2 "fcba"
  describe "handleGuess" $ do
    it "should be bad guess" $ do
      puzzle' <- handleGuess puzzle 'a'
      puzzle' `shouldBe` puzzle
    it "should do nothing" $ do
      puzzle' <- handleGuess puzzle 'd'
      puzzle' `shouldNotBe` puzzle