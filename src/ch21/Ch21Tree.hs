module Ch21Tree where

import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, Testable, elements, sample)
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (traversable)

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 x t2) = Node (f <$> t1) (f x) (f <$> t2)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 x t2) = foldMap f t1 <> f x <> foldMap f t2

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 x t2) = Node <$> traverse f t1 <*> f x <*> traverse f t2

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = go
    where
      go :: Arbitrary a => Gen (Tree a)
      go = do
        t1 <- go
        x <- arbitrary
        y <- arbitrary
        t2 <- go
        elements [Empty, Leaf x, Node t1 y t2]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

runQc :: IO ()
runQc = quickBatch $ traversable (undefined :: Tree ([Int], [Int], [Int]))

giveSample :: IO ()
giveSample = sample (arbitrary :: Gen (Tree Int))