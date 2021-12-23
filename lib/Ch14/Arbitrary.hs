module Ch14.Arbitrary where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data Pair a b = Pair a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    Pair a <$> arbitrary

data Sum a b = First a | Second b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]