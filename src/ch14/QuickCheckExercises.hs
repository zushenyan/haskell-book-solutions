module QuickCheckExercises where

import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck

-- 1.
half :: (Fractional a) => a -> a
half = (/ 2)

halfIdentity :: (Fractional a) => a -> a
halfIdentity = (* 2) . half

run1 :: IO ()
run1 = quickCheck prop_half
  where
    prop_half :: Double -> Bool
    prop_half x = x == halfIdentity x

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

run2 :: IO ()
run2 = quickCheck prop_list
  where
    prop_list :: [Int] -> Bool
    prop_list list = listOrdered (sort list)

-- 3.
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

run3 :: IO ()
run3 = do
  quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (plusCommutative :: Int -> Int -> Bool)
  return ()

-- 4.
mulAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
mulAssociative x y z = x * (y * z) == (x * y) * z

mulCommutative :: (Eq a, Num a) => a -> a -> Bool
mulCommutative x y = x * y == y * x

run4 :: IO ()
run4 = do
  quickCheck (mulAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (mulCommutative :: Int -> Int -> Bool)
  return ()

-- 5.
myQuotRem :: Integral a => a -> a -> Bool
myQuotRem x y = (quot x y) * y + (rem x y) == x

myDivMod :: Integral a => a -> a -> Bool
myDivMod x y = (div x y) * y + (mod x y) == x

run5 :: IO ()
run5 = do
  quickCheck prop_myQuotRem
  quickCheck prop_myDivMod
  return ()
  where
    prop_myQuotRem :: NonZero Int -> NonZero Int -> Bool
    prop_myQuotRem (NonZero x) (NonZero y) = myQuotRem x y
    prop_myDivMod :: NonZero Int -> NonZero Int -> Bool
    prop_myDivMod (NonZero x) (NonZero y) = myDivMod x y

-- 6.
powerAssociative :: (Integral b1, Integral b2, Num a, Eq a) => a -> b1 -> b2 -> Bool
powerAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: Integral b => b -> b -> Bool
powerCommutative x y = x ^ y == y ^ x

run6 :: IO ()
run6 = do
  quickCheck (powerAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (powerCommutative :: Int -> Int -> Bool)
  return ()

-- 7.
reverseListIdentity :: (Eq a) => [a] -> Bool
reverseListIdentity x = (id x) == (reverse . reverse $ x)

run7 :: IO ()
run7 = quickCheck (reverseListIdentity :: [Int] -> Bool)

-- 8.
run8 :: IO ()
run8 = do
  quickCheck prop_dollar
  quickCheck prop_compose
  where
    prop_dollar :: Int -> Bool
    prop_dollar x = id x == (id $ x)
    prop_compose :: Int -> Bool
    prop_compose x = (id (id x)) == (id . id $ x)

-- 9.
testFoldr1 :: (Eq a) => a -> [a] -> Bool
testFoldr1 x y = foldr (:) [x] y == y ++ [x]

testFoldr2 :: (Eq a, Foldable t) => t [a] -> Bool
testFoldr2 x = foldr (++) [] x == concat x

run9 :: IO ()
run9 = do
  quickCheck (testFoldr1 :: Int -> [Int] -> Bool)
  quickCheck (testFoldr2 :: [[String]] -> Bool)

-- 10.
f :: Int -> [a] -> Bool
f n xs = length (take n xs) == n

run10 :: IO ()
run10 = quickCheck (f :: Int -> String -> Bool)

-- 11.
f' :: (Eq a, Read a, Show a) => a -> Bool
f' x = (read (show x)) == x

run11 :: IO ()
run11 = do
  quickCheck (f' :: String -> Bool)
  quickCheck (f' :: Int -> Bool)
  quickCheck (f' :: Char -> Bool)

-- Failure
-- because the floats are not precise due to how it stores in the memory.
sq :: Num a => a -> a
sq x = x * x

sqId :: Double -> Double
sqId = sq . sqrt

runFailure :: IO ()
runFailure = quickCheck prop_sqId
  where
    prop_sqId :: Double -> Bool
    prop_sqId x = sqId x == x

-- Idempotence
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs) = toUpper x : xs

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

-- 1.
idemF :: String -> Bool
idemF x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

runIdem1 :: IO ()
runIdem1 = quickCheck idemF

-- 2.
idemF' :: Ord a => [a] -> Bool
idemF' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

runIdem2 :: IO ()
runIdem2 = quickCheck (idemF' :: [String] -> Bool)

-- Make a Gen random generator for the datatype
-- 1.
data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

-- 2.
genFool' :: Gen Fool
genFool' = frequency [(2, return Fulse), (1, return Frue)]