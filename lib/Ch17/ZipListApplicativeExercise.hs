module Ch17.ZipListApplicativeExercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (f <$> xs)

instance Applicative ZipList' where
  pure a = ZipList' [a]
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ go fs xs
    where
      go :: [a -> b] -> [a] -> [b]
      go [] _ = []
      go _ [] = []
      go fs [x] = fs <*> pure x
      go [f] xs = f <$> xs
      go (f : fs) (x : xs) = f x : go fs xs

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs in take 3000 l
      ys' = let (ZipList' l) = ys in take 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

run :: IO ()
run = do
  let z = ZipList' [(+ 9), (* 2), (+ 8)]
      z' = ZipList' [1 .. 3]
  print $ z <*> z'
  print $ z <*> pure 1

main :: IO ()
main = do
  quickBatch $ applicative app
  where
    app :: ZipList' (Int, Int, Int)
    app = undefined