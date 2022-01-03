module Ch18.ChapterExercises1 where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

test1 :: IO ()
test1 = quickBatch $ monad (undefined :: Nope (Int, Int, Int))

-- 2.
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft $ f a

instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PLeft f) (PLeft a) = PLeft $ f a
  (<*>) (PLeft _) (PRight a) = PRight a
  (<*>) (PRight f) (PLeft _) = PRight f
  (<*>) (PRight f) (PRight _) = PRight f

instance Monad (BahEither b) where
  return = pure
  (>>=) (PLeft a) f = f a
  (>>=) (PRight a) _ = PRight a

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

test2 :: IO ()
test2 = quickBatch $ monad (undefined :: BahEither (Int, Int, Int) (Int, Int, Int))

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

test3 :: IO ()
test3 = quickBatch $ monad (undefined :: Identity (Int, Int, Int))

-- 4.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (f <$> l)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) `append` (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x l) f = f x `append` (l >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

test4 :: IO ()
test4 = do
  quickBatch $ monad (undefined :: List (Int, Int, Int))

run4 :: IO ()
run4 = do
  print $ Cons 1 Nil >>= (\x -> Cons (x + 1) Nil)