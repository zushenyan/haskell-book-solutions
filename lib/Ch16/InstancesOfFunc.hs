module Ch16.InstancesOfFunc where

import Test.QuickCheck

fi :: (Functor f, Eq (f a)) => f a -> Bool
fi f = fmap id f == f

fc :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
fc (Fun _ f) (Fun _ g) x = fmap g (fmap f x) == fmap (g . f) x

-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type T1 = Identity Int

test1 :: IO ()
test1 = do
  quickCheck (fi :: T1 -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Float -> T1 -> Bool)

-- 2.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

type T2 = Pair Int

test2 :: IO ()
test2 = do
  quickCheck (fi :: T2 -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Float -> T2 -> Bool)

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type T3 = Two Char Int

test3 :: IO ()
test3 = do
  quickCheck (fi :: T3 -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Float -> T3 -> Bool)

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type T4 = Three Float Char Int

test4 :: IO ()
test4 = do
  quickCheck (fi :: T4 -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Float -> T4 -> Bool)

-- 5.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

type T5 = Three' Float Int

test5 :: IO ()
test5 = do
  quickCheck (fi :: T5 -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Float -> T5 -> Bool)

-- 6.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type T6 = Four Bool Float Char Int

test6 :: IO ()
test6 = do
  quickCheck (fi :: T6 -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Float -> T6 -> Bool)

-- 7.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c $ f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type T7 = Four' Bool Int

test7 :: IO ()
test7 = do
  quickCheck (fi :: T7 -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Float -> T7 -> Bool)

-- 8.
data Trivial = Trivial

-- ans: No, because instnace Functor takes * -> * as its kind, however the kind for Trivial is * which doesn't apply to the functor constrait.