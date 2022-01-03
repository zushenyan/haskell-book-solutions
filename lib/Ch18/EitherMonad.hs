module Ch18.EitherMonad where

import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (monad)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (<*>) (Second f) (Second a) = Second $ f a
  (<*>) (First f) (Second _) = First f
  (<*>) (Second _) (First a) = First a
  (<*>) (First a) (First _) = First a

instance Monad (Sum a) where
  return = pure
  (>>=) (Second a) f = f a
  (>>=) (First a) _ = First a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

run :: IO ()
run = do
  let f :: (Monad m, Num a) => a -> m a
      f = return . (+ 1)
      g = (+ 1)
  print $ (+ 1) <$> First 1
  print ((+ 1) <$> Second 1 :: Sum Int Int)
  print (First g <*> First g :: Sum (Int -> Int) (Int -> Int))
  print (First g <*> Second 1 :: Sum (Int -> Int) Int)
  print $ Second (+ 1) <*> First 1
  print ((Second (+ 1) :: Sum Int (Int -> Int)) <*> (Second 1 :: Sum Int Int))
  print $ First 1 >>= f
  print (Second 1 >>= f :: Sum Int Int)

test :: IO ()
test = do
  quickBatch $ monad (undefined :: Sum (Int, Int, Int) (Int, Int, Int))