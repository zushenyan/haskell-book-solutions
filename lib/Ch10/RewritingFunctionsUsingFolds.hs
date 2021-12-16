module Ch10.RewritingFunctionsUsingFolds where

-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (const . f) False

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (const . (== a)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (== a)

-- 4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\v acc -> f v : acc) []

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\v acc -> if f v then v : acc else acc) []

-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\v acc -> f v ++ acc) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f arr = foldr (\x acc -> if f x acc == GT then x else acc) (head arr) arr

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f arr = foldr (\x acc -> if f x acc == LT then x else acc) (head arr) arr