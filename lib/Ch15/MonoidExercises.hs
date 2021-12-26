module Ch15.MonoidExercises where

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

mli :: (Eq m, Monoid m) => m -> Bool
mli a = mempty <> a == a

mri :: (Eq m, Monoid m) => m -> Bool
mri a = a == mempty <> a

-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type T1 = Trivial

test1 :: IO ()
test1 = do
  quickCheck (sa :: T1 -> T1 -> T1 -> Bool)
  quickCheck (mli :: T1 -> Bool)
  quickCheck (mri :: T1 -> Bool)

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity $ x <> y

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type T2 = Identity (Sum Int)

test2 :: IO ()
test2 = do
  quickCheck (sa :: T2 -> T2 -> T2 -> Bool)
  quickCheck (mli :: T2 -> Bool)
  quickCheck (mri :: T2 -> Bool)

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type T3 = Two (Sum Int) (Product Int)

test3 :: IO ()
test3 = do
  quickCheck (sa :: T3 -> T3 -> T3 -> Bool)
  quickCheck (mli :: T3 -> Bool)
  quickCheck (mri :: T3 -> Bool)

-- 4.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj False) _ = BoolConj False
  (<>) _ (BoolConj False) = BoolConj False
  (<>) _ _ = BoolConj True

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type T4 = BoolConj

test4 :: IO ()
test4 = do
  quickCheck (sa :: T4 -> T4 -> T4 -> Bool)
  quickCheck (mli :: T4 -> Bool)
  quickCheck (mri :: T4 -> Bool)

-- 5.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _ _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type T5 = BoolDisj

test5 :: IO ()
test5 = do
  quickCheck (sa :: T5 -> T5 -> T5 -> Bool)
  quickCheck (mli :: T5 -> Bool)
  quickCheck (mri :: T5 -> Bool)

-- 6.
newtype Combine a b = Combine {unCombine :: a -> b}

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (\x -> f x <> g x)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)

run6 :: IO ()
run6 = do
  let f :: Combine Int (Sum Int)
      f = Combine $ \n -> Sum (n + 1)
  print $ unCombine (f `mappend` mempty) 1

type T6 = Fun Int (Sum Int)

test6 :: IO ()
test6 = do
  quickCheck saF
  quickCheck mliF
  quickCheck mriF
  where
    saF :: T6 -> T6 -> T6 -> Int -> Bool
    saF (Fun _ f) (Fun _ g) (Fun _ h) v = unCombine (a <> (b <> c)) v == unCombine ((a <> b) <> c) v
      where
        a = Combine f
        b = Combine g
        c = Combine h
    mliF :: T6 -> Int -> Bool
    mliF (Fun _ f) v = unCombine (mempty <> a) v == unCombine a v
      where
        a = Combine f
    mriF :: T6 -> Int -> Bool
    mriF (Fun _ f) v = unCombine a v == unCombine (mempty <> a) v
      where
        a = Combine f

-- 7.
newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup a => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f . g)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp (<> mempty)

type T7 = Fun (Sum Int) (Sum Int)

test7 :: IO ()
test7 = do
  quickCheck saF
  quickCheck mliF
  quickCheck mriF
  where
    saF :: T7 -> T7 -> T7 -> Sum Int -> Bool
    saF (Fun _ f) (Fun _ g) (Fun _ h) v = unComp (a <> (b <> c)) v == unComp ((a <> b) <> c) v
      where
        a = Comp f
        b = Comp g
        c = Comp h
    mliF :: T7 -> Sum Int -> Bool
    mliF (Fun _ f) v = unComp (mempty <> a) v == unComp a v
      where
        a = Comp f
    mriF :: T7 -> Sum Int -> Bool
    mriF (Fun _ f) v = unComp a v == unComp (mempty <> a) v
      where
        a = Comp f

-- 8.
newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem go
    where
      go s = (fst fr <> fst gr, snd gr)
        where
          fr = f s
          gr = g $ snd fr

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))

run8 :: IO ()
run8 = do
  let f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

type T8 = Fun Int (String, Int)

test8 :: IO ()
test8 = do
  quickCheck saF
  quickCheck mliF
  quickCheck mriF
  where
    saF :: T8 -> T8 -> T8 -> Int -> Bool
    saF (Fun _ f) (Fun _ g) (Fun _ h) v = runMem (a <> (b <> c)) v == runMem ((a <> b) <> c) v
      where
        a = Mem f
        b = Mem g
        c = Mem h
    mliF :: T8 -> Int -> Bool
    mliF (Fun _ f) v = runMem (mempty <> a) v == runMem a v
      where
        a = Mem f
    mriF :: T8 -> Int -> Bool
    mriF (Fun _ f) v = runMem a v == runMem (mempty <> a) v
      where
        a = Mem f