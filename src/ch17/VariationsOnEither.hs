module VariationsOnEither where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Failure' e | Success' a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap f (Success' a) = Success' (f a)
  fmap _ (Failure' e) = Failure' e

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Success' f) (Success' x) = Success' (f x)
  (<*>) (Failure' x) (Failure' y) = Failure' (x <> y)
  (<*>) (Failure' x) _ = Failure' x
  (<*>) _ (Failure' x) = Failure' x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure' e, Success' a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

test :: IO ()
test = do
  print $ f1 == Success' 2
  print $ f2 == Failure' ["foo"]
  print $ f3 == Failure' ["foo"]
  print $ f4 == Failure' ["foo", "bar"]
  where
    f1 :: Validation [String] Int
    f1 = Success' (+ 1) <*> Success' 1
    f2 :: Validation [String] Int
    f2 = Success' (+ 1) <*> Failure' ["foo"]
    f3 :: Validation [String] Int
    f3 = Failure' ["foo"] <*> Success' (+ 1)
    f4 :: Validation [String] Int
    f4 = Failure' ["foo"] <*> Failure' ["bar"]

runQc :: IO ()
runQc = quickBatch $ applicative go
  where
    go :: Validation (String, String, String) (Int, Int, Int)
    go = undefined