module Ch17.ChapterExercises2 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

test1 :: IO ()
test1 = quickBatch $ applicative go
  where
    go :: Pair (Int, Int, Int)
    go = undefined

-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two m f) (Two a b) = Two (m <> a) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

test2 :: IO ()
test2 = quickBatch $ applicative go
  where
    go :: Two (Sum Int, Sum Int, Sum Int) (Int, Int, Int)
    go = undefined

-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three ma mb f) (Three a b c) = Three (ma <> a) (mb <> b) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

test3 :: IO ()
test3 = quickBatch $ applicative go
  where
    go :: Three (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int) (Int, Int, Int)
    go = undefined

-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
  pure a = Three' mempty a a
  (<*>) (Three' ma f g) (Three' a b b') = Three' (ma <> a) (f b) (g b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

test4 :: IO ()
test4 = quickBatch $ applicative go
  where
    go :: Three' (Sum Int, Sum Int, Sum Int) (Int, Int, Int)
    go = undefined

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four ma mb mc f) (Four a b c d) = Four (ma <> a) (mb <> b) (mc <> c) $ f d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

test5 :: IO ()
test5 = quickBatch $ applicative go
  where
    go :: Four (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int) (Sum Int, Sum Int, Sum Int) (Int, Int, Int)
    go = undefined

-- 6.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' $ f b

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' ma ma' ma'' f) (Four' a a' a'' b) = Four' (ma <> a) (ma' <> a') (ma'' <> a'') $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

test6 :: IO ()
test6 = quickBatch $ applicative go
  where
    go :: Four' (Sum Int, Sum Int, Sum Int) (Int, Int, Int)
    go = undefined