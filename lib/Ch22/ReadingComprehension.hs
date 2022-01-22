{-# LANGUAGE InstanceSigs #-}

module Ch22.ReadingComprehension where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Reader r a = Reader {runReader :: r -> a} deriving (Show)

instance Eq (Reader r a) where
  (==) (Reader _) (Reader _) = True

-- 1.
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- 2.
asks :: (r -> a) -> Reader r a
asks = Reader

-- 3.
instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r $ ra r

instance (CoArbitrary r, Arbitrary a) => Arbitrary (Reader r a) where
  arbitrary = Reader <$> arbitrary

instance (Eq r, Eq a) => EqProp (Reader r a) where
  (=-=) = eq

test3 :: IO ()
test3 = quickBatch $ applicative (undefined :: Reader (Int, Int, [Int]) (Int, Int, [Int]))