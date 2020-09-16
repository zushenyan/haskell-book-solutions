module Ch18Exercises where

import Control.Monad (join, liftM2)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (monad, monoid, semigroup)

-- part 1
-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance (Arbitrary a) => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

runQc1 :: IO ()
runQc1 = quickBatch $ monad go
  where
    go :: Nope (Int, Int, Int)
    go = undefined

-- 2.
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight x) = PRight x
  fmap f (PLeft x) = PLeft $ f x

instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PRight x) _ = PRight x
  (<*>) _ (PRight x) = PRight x
  (<*>) (PLeft f) (PLeft x) = PLeft $ f x

instance Monad (BahEither b) where
  return = pure
  (>>=) (PRight x) _ = PRight x
  (>>=) (PLeft x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [PLeft x, PRight y]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

runQc2 :: IO ()
runQc2 = quickBatch $ monad go
  where
    go :: BahEither (Int, Int, Int) (Int, Int, Int)
    go = undefined

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

runQc3 :: IO ()
runQc3 = quickBatch $ monad go
  where
    go :: Identity (Int, Int, Int)
    go = undefined

-- 4.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) (Cons x Nil) y = Cons x y
  (<>) (Cons x xs) y = Cons x (xs <> y)
  (<>) Nil x = x

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) (Cons f fs) l = (f <$> l) <> (fs <*> l)
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil

instance Monad List where
  return = pure
  (>>=) (Cons x xs) f = f x <> (xs >>= f)
  (>>=) Nil _ = Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = go
    where
      go :: Arbitrary a => Gen (List a)
      go = do
        x <- arbitrary
        l <- go
        elements [Nil, Cons x l]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

test4 :: IO ()
test4 = do
  print $ list >>= f
  print $ f <$> list
  where
    list :: List Int
    list = Cons 1 (Cons 2 (Cons 3 Nil))
    f :: Int -> List Int
    f x = return $ x + 1

runQc4 :: IO ()
runQc4 = do
  quickBatch $ monoid go
  quickBatch $ monad go
  where
    go :: List (Int, Int, Int)
    go = undefined

-- part 2
-- 1.
j :: Monad m => m (m a) -> m a
j = join

spec1 :: IO ()
spec1 = hspec $ do
  describe "j" $ do
    it "[[1, 2], [], [3]]" $ do j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
    it "Just (Just 1)" $ do j (Just (Just 1)) `shouldBe` Just 1
    it "Just Nothing" $ do (j (Just Nothing) :: Maybe Int) `shouldBe` Nothing
    it "Nothing" $ do (j Nothing :: Maybe Int) `shouldBe` Nothing

--  2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = liftM2 (++) ((: []) <$> f x) (meh xs f)

-- 6.
flipType :: Monad m => [m a] -> m [a]
flipType x = meh x id