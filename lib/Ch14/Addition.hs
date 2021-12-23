module Ch14.Addition where

import Ch8.FixingDividedBy (dividedBy)
import Test.Hspec
import Test.QuickCheck

tirvialInt :: Gen Int
tirvialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1 .. 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements . enumFrom $ LT

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, a]

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return a)]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1 " $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` Just (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` Just (4, 2)
    it "x + 1 is always greater than x" $ do
      property (\x -> x + 1 > (x :: Int))

runQC :: IO ()
runQC = quickCheck prop_additionGreater