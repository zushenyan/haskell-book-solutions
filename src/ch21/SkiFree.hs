{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, Testable, elements, sample')
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (traversable)

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) $ f a

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

runQc :: IO ()
runQc = quickBatch $ traversable (undefined :: S Maybe ([Int], [Int], [Int]))

main :: IO [S [] Int]
main = sample' (arbitrary :: Gen (S [] Int))