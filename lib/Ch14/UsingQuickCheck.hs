module Ch14.UsingQuickCheck where

import Data.List (sort)
import Test.QuickCheck

-- 1.
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

test1 :: IO ()
test1 = quickCheck $ property (\x -> x == halfIdentity x)

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

test2 :: IO ()
test2 = quickCheck $ property (\x -> listOrdered . sort $ (x :: [Int]))

-- 3.
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

test3 :: IO ()
test3 = do
  quickCheck $ property (\x y z -> plusAssociative (x :: Int) y z)
  quickCheck $ property (\x y -> plusCommutative (x :: Int) y)

-- 4.
mulAss :: (Eq a, Fractional a) => a -> a -> a -> Bool
mulAss x y z = x + (y + z) == (x + y) + z

mulComm :: (Eq a, Fractional a) => a -> a -> Bool
mulComm x y = x + y == y + x

test4 :: IO ()
test4 = do
  quickCheck $ property (\x y z -> mulAss (x :: Float) y z)
  quickCheck $ property (\x y -> mulComm (x :: Float) y)

-- 5.
test5 :: IO ()
test5 = do
  quickCheck $ property t1
  quickCheck $ property t2
  where
    t1 :: NonZero Int -> NonZero Int -> Bool
    t1 (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x
    t2 :: NonZero Int -> NonZero Int -> Bool
    t2 (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x

--  6.
expAss :: (Eq a, Integral a) => a -> a -> a -> Bool
expAss x y z = x ^ (y ^ z) == (x ^ y) ^ z

expComm :: (Eq a, Integral a) => a -> a -> Bool
expComm x y = x ^ y == y ^ x

test6 :: IO ()
test6 = do
  quickCheck $ property (\x y z -> expAss (x :: Int) y z)
  quickCheck $ property (\x y -> expComm (x :: Int) y)

-- 7.
test7 :: IO ()
test7 = quickCheck $ property (\x -> (reverse . reverse $ x) == (x :: [Int]))

-- 8.
test8 :: IO ()
test8 = quickCheck $ property (\x -> id (x :: Char) == id (id x))

-- 9.
test9 :: IO ()
test9 = do
  quickCheck $ property (\x -> foldr (:) [] x == [] ++ (x :: String))
  quickCheck $ property (\x -> foldr (++) [] x == concat (x :: [String]))

-- 10.
test10 :: IO ()
test10 = quickCheck $ property f
  where
    f :: Int -> String -> Bool
    f n xs = length (take n xs) == n

-- 11.
test11 :: IO ()
test11 = quickCheck $ property (\x -> (read . show $ x) == (x :: String))