module Ch20Exercises where

import Data.Foldable ()
import Data.Monoid (Sum)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (foldable)

-- 1.
data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

runQc1 :: IO ()
runQc1 = quickBatch $ foldable (undefined :: Constant (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))

-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    Two x <$> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

runQc2 :: IO ()
runQc2 = quickBatch $ foldable (undefined :: Two (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))

-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    Three x y <$> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

runQc3 :: IO ()
runQc3 = quickBatch $ foldable (undefined :: Three (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))

-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three' a b <$> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

runQc4 :: IO ()
runQc4 = quickBatch $ foldable (undefined :: Three' (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))

-- 5.
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four' a b c <$> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

runQc5 :: IO ()
runQc5 = quickBatch $ foldable (undefined :: Four' (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))

-- 6.
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\v -> if f v then pure v else mempty)
