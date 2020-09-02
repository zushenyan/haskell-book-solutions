module MultiplicationTest where

import           Test.Hspec

mul :: (Eq a, Num a) => a -> a -> a
mul _ 0 = 0
mul x 1 = x
mul x y = x + mul x (y - 1)

main :: IO ()
main = hspec $ describe "MultiplicationTest" $ do
  it "should be 6" $ mul 2 3 == 6 `shouldBe` True
  it "should be x" $ mul 3 1 == 3 `shouldBe` True
  it "should be 0" $ mul 3 0 == 0 `shouldBe` True
