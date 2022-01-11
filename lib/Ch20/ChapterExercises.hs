module Ch20.ChapterExercises where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
newtype Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

test1 :: IO ()
test1 = quickBatch $ foldable (undefined :: Constant (Int, Int, Sum Int, Int, Int) (Int, Int, Sum Int, Int, Int))

-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

test2 :: IO ()
test2 = quickBatch $ foldable (undefined :: Two (Int, Int, Sum Int, Int, Int) (Int, Int, Sum Int, Int, Int))

-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

test3 :: IO ()
test3 = quickBatch $ foldable (undefined :: Three (Int, Int, Sum Int, Int, Int) (Int, Int, Sum Int, Int, Int) (Int, Int, Sum Int, Int, Int))

-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

test4 :: IO ()
test4 = quickBatch $ foldable (undefined :: Three' (Int, Int, Sum Int, Int, Int) (Int, Int, Sum Int, Int, Int))

-- 5.
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

test5 :: IO ()
test5 = quickBatch $ foldable (undefined :: Four' (Int, Int, Sum Int, Int, Int) (Int, Int, Sum Int, Int, Int))

-- 6.
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\v -> if f v then pure v else mempty)