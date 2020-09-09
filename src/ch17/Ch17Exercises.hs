module Ch17Exercises where

import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair fx fy) (Pair x y) = Pair (fx x) (fy y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

runQc1 :: IO ()
runQc1 = quickBatch $ applicative go
  where
    go :: Pair (Int, Int, Int)
    go = undefined

-- 2.
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two x fy) (Two x' y') = Two (x <> x') (fy y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

runQc2 :: IO ()
runQc2 = quickBatch $ applicative go
  where
    go :: Two ([Int], [Int], [Int]) (Int, Int, Int)
    go = undefined

-- 3.
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three x y fz) (Three x' y' z') = Three (x <> x') (y <> y') (fz z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

runQc3 :: IO ()
runQc3 = quickBatch $ applicative go
  where
    go :: Three ([Int], [Int], [Int]) ([Int], [Int], [Int]) (Int, Int, Int)
    go = undefined

-- 4.
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a fb fc) (Three' a' b' c') = Three' (a <> a') (fb b') (fc c')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

runQc4 :: IO ()
runQc4 = quickBatch $ applicative go
  where
    go :: Three' ([Int], [Int], [Int]) (Int, Int, Int)
    go = undefined

-- 5.
data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c fd) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (fd d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

runQc5 :: IO ()
runQc5 = quickBatch $ applicative go
  where
    go :: Four ([Int], [Int], [Int]) ([Int], [Int], [Int]) ([Int], [Int], [Int]) (Int, Int, Int)
    go = undefined

-- 6.
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a b c f) (Four' a' b' c' d') = Four' (a <> a') (b <> b') (c <> c') (f d')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

runQc6 :: IO ()
runQc6 = quickBatch $ applicative go
  where
    go :: Four' ([Int], [Int], [Int]) (Int, Int, Int)
    go = undefined

-- combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)