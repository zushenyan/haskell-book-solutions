module Ch9.WritingYourOwnStandardFunctions where

import Data.Bool (bool)

-- 1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs)
  | x = True
  | otherwise = myOr xs

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs)
  | f x = True
  | otherwise = myAny f xs

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs)
  | a == x = True
  | otherwise = myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' a b = any (== a) b

-- 4.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- 5.
squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

-- 6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

-- 7.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (++ [])

-- 8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f arr = go arr (head arr)
  where
    go [] temp = temp
    go (x : xs) temp = go xs (bool temp x (f x temp == GT))

-- 9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f arr = go arr (head arr)
  where
    go [] temp = temp
    go (x : xs) temp = go xs (bool temp x (f x temp == LT))

-- 10.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
