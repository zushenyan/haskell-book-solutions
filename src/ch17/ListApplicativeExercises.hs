module ListApplicativeExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = append (f <$> xs) (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = go
    where
      go :: Arbitrary a => Gen (List a)
      go = do
        x <- arbitrary
        l <- go
        elements [Nil, Cons x l]

instance Eq a => EqProp (List a) where
  (=-=) = eq

test1 :: IO ()
test1 =
  let f = Cons (+ 1) (Cons (* 2) Nil)
      v = Cons 1 (Cons 2 Nil)
   in do
        print $ f <*> v
        print . show $ (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

runQc1 :: IO ()
runQc1 = quickBatch $ applicative go
  where
    go :: List (Int, Int, Int)
    go = undefined

-- ZipList
newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (f <$> xs)

instance Applicative ZipList' where
  pure x = ZipList' [x]
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ go fs xs
    where
      go :: [a -> b] -> [a] -> [b]
      go [] _ = []
      go _ [] = []
      go fs [x] = fs <*> pure x
      go [f] xs = f <$> xs
      go (f : fs) (x : xs) = f x : go fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  (=-=) = eq

test2 :: IO ()
test2 =
  let a = ZipList' [(+ 9), (* 2), (+ 8)]
   in do
        print $ a <*> ZipList' [1 .. 3]
        print $ a <*> pure 1
        print $ (a <*> ZipList' [1 .. 3]) == ZipList' [10, 4, 11]
        print $ (a <*> pure 1) == ZipList' [10, 2, 9]

runQc2 :: IO ()
runQc2 = quickBatch $ applicative go
  where
    go :: ZipList' (Int, Int, Int)
    go = undefined