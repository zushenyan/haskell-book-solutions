module EitherMonad where

import Test.QuickCheck (Arbitrary (arbitrary), elements)
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (monad)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First x) _ = First x
  (<*>) _ (First x) = First x
  (<*>) (Second f) (Second y) = Second $ f y

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = First x
  (>>=) (Second x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [First x, Second y]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

run :: IO ()
run = do
  print $ (First 1 >>= f) == First 1
  print $ (Second 1 >>= f) == Second 2
  where
    f :: Int -> Sum Int Int
    f x = return $ x + 1

runQc :: IO ()
runQc = quickBatch $ monad go
  where
    go :: Sum (Int, Int, Int) (Int, Int, Int)
    go = undefined