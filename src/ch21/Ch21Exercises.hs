module Ch21Exercises where

import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, Testable, elements, sample')
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (traversable)

-- 1.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

runQc1 :: IO ()
runQc1 = quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))

-- 2.
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = Constant <$> pure a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

runQc2 :: IO ()
runQc2 = quickBatch $ traversable (undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int]))

-- 3.
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    elements [Nada, Yep x]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

runQc3 :: IO ()
runQc3 = quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))

-- 4.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = go
    where
      go :: Arbitrary a => Gen (List a)
      go = do
        x <- arbitrary
        y <- go
        elements [Nil, Cons x y]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

runQc4 :: IO ()
runQc4 = quickBatch $ traversable (undefined :: List (Int, Int, [Int]))

-- 5.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

runQc5 :: IO ()
runQc5 = quickBatch $ traversable (undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int]))

-- 6.
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

runQc6 :: IO ()
runQc6 = quickBatch $ traversable (undefined :: Pair (Int, Int, [Int]) (Int, Int, [Int]))

-- 7.
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f (Big a b c) = f b <> f c

instance Traversable (Big a) where
  traverse f (Big a b c) = Big a <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

runQc7 :: IO ()
runQc7 = quickBatch $ traversable (undefined :: Big (Int, Int, [Int]) (Int, Int, [Int]))

-- 8.
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

runQc8 :: IO ()
runQc8 = quickBatch $ traversable (undefined :: Bigger (Int, Int, [Int]) (Int, Int, [Int]))
