module Ch15.SemigroupExercises where

import Data.Monoid
import Data.Semigroup (All, Any, Product, Sum (Sum))
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Fun (..),
    oneof,
    quickCheck,
  )

sa :: (Eq m, Semigroup m) => m -> m -> m -> Bool
sa a b c = a <> (b <> c) == (a <> b) <> c

-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type T1 = Trivial

test1 :: IO ()
test1 = quickCheck (sa :: T1 -> T1 -> T1 -> Bool)

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity $ x <> y

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type T2 = Identity (Sum Int)

test2 :: IO ()
test2 = quickCheck (sa :: T2 -> T2 -> T2 -> Bool)

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type T3 = Two (Sum Int) (Product Int)

test3 :: IO ()
test3 = quickCheck (sa :: T3 -> T3 -> T3 -> Bool)

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three x y z) (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type T4 = Three (Sum Int) (Product Int) All

test4 :: IO ()
test4 = quickCheck (sa :: T4 -> T4 -> T4 -> Bool)

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four x y z a) (Four x' y' z' a') = Four (x <> x') (y <> y') (z <> z') (a <> a')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type T5 = Four (Sum Int) (Product Int) All Any

test5 :: IO ()
test5 = quickCheck (sa :: T5 -> T5 -> T5 -> Bool)

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj False) _ = BoolConj False
  (<>) _ (BoolConj False) = BoolConj False
  (<>) _ _ = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type T6 = BoolConj

test6 :: IO ()
test6 = quickCheck (sa :: T6 -> T6 -> T6 -> Bool)

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _ _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type T7 = BoolDisj

test7 :: IO ()
test7 = quickCheck (sa :: T7 -> T7 -> T7 -> Bool)

-- 8.
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (<>) (Fst a) (Fst b) = Fst b
  (<>) (Snd a) _ = Snd a
  (<>) _ (Snd b) = Snd b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    oneof [Fst <$> arbitrary, Snd <$> arbitrary]

type T8 = Or (Sum Int) (Product Float)

test8 :: IO ()
test8 = quickCheck (sa :: T8 -> T8 -> T8 -> Bool)

-- 9.
newtype Combine a b = Combine {unCombine :: a -> b}

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (\x -> f x <> g x)

run9 :: IO ()
run9 = do
  let f :: Combine Int (Sum Int)
      f = Combine $ \n -> Sum (n + 1)
      g :: Combine Int (Sum Int)
      g = Combine $ \n -> Sum (n - 1)
  print $ unCombine (f <> g) 0
  print $ unCombine (f <> g) 1
  print $ unCombine (f <> f) 1
  print $ unCombine (g <> f) 1

type T9 = Fun Int (Sum Int)

test9 :: IO ()
test9 = quickCheck semigroupAssocF
  where
    semigroupAssocF :: T9 -> T9 -> T9 -> Int -> Bool
    semigroupAssocF (Fun _ f) (Fun _ g) (Fun _ h) v = unCombine (a <> (b <> c)) v == unCombine ((a <> b) <> c) v
      where
        a = Combine f
        b = Combine g
        c = Combine h

-- 10.
newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup a => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f . g)

type T10 = Fun (Sum Int) (Sum Int)

test10 :: IO ()
test10 = quickCheck semigroupAssocF
  where
    semigroupAssocF :: T10 -> T10 -> T10 -> Sum Int -> Bool
    semigroupAssocF (Fun _ f) (Fun _ g) (Fun _ h) v = unComp (a <> (b <> c)) v == unComp ((a <> b) <> c) v
      where
        a = Comp f
        b = Comp g
        c = Comp h

-- 11.
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure x) (Failure y) = Failure (x <> y)
  (<>) (Success x) _ = Success x
  (<>) _ (Success x) = Success x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Success <$> arbitrary, Failure <$> arbitrary]

type T11 = Validation String Int

test11 :: IO ()
test11 = quickCheck (sa :: T11 -> T11 -> T11 -> Bool)

run11 :: IO ()
run11 = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2