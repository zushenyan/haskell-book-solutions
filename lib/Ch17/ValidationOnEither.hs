module Ch17.ValidationOnEither where

import Data.Monoid
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Test.QuickCheck.Checkers (EqProp ((=-=)), eq, quickBatch)
import Test.QuickCheck.Classes (applicative)

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success b) = Success $ f b
  (<*>) (Failure e) (Failure e') = Failure $ e <> e'
  (<*>) (Failure e) _ = Failure e
  (<*>) _ (Failure e) = Failure e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

run :: IO ()
run = do
  print (Success (+ 1) <*> Success 1 :: Validation [Int] Int)
  print $ Success (+ 1) <*> Failure [1]
  print (Failure [1] <*> Success (+ 1) :: Validation [Int] Int)
  print (Failure [1] <*> Failure [2] :: Validation [Int] Int)

main :: IO ()
main = quickBatch $ applicative go
  where
    go :: Validation (Sum Int, Sum Int, Sum Int) (Int, Int, Int)
    go = undefined
