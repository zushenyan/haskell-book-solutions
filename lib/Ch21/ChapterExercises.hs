module Ch21.ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

test1 :: IO ()
test1 = quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))

-- 2.
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

test2 :: IO ()
test2 = quickBatch $ traversable (undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int]))

-- 3.
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

test3 :: IO ()
test3 = quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))

-- 4.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

test4 :: IO ()
test4 = quickBatch $ traversable (undefined :: List (Int, Int, [Int]))

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

test5 :: IO ()
test5 = quickBatch $ traversable (undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int]))

-- 6.
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

test6 :: IO ()
test6 = quickBatch $ traversable (undefined :: Pair (Int, Int, [Int]) (Int, Int, [Int]))

-- 7.
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

test7 :: IO ()
test7 = quickBatch $ traversable (undefined :: Big (Int, Int, [Int]) (Int, Int, [Int]))

-- 8.
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

test8 :: IO ()
test8 = quickBatch $ traversable (undefined :: Bigger (Int, Int, [Int]) (Int, Int, [Int]))

-- 9.
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

test9 :: IO ()
test9 = quickBatch $ traversable (undefined :: S [] (Int, Int, [Int]))

run9 :: IO [S [] Int]
run9 = sample' (arbitrary :: Gen (S [] Int))

-- 10.
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = oneof [return Empty, Leaf <$> arbitrary, Node <$> arbitrary <*> arbitrary <*> arbitrary]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

test10 :: IO ()
test10 = quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))

run10 :: IO [Tree Int]
run10 = sample' (arbitrary :: Gen (Tree Int))
