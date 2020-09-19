module LibraryFunctions where

import Data.Foldable (fold, foldMap, toList)
import Data.Monoid

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\a -> Any $ x == a)

-- 4.
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' x
  | null x = Nothing
  | otherwise = Just $ foldr min (head . toList $ x) x

-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' x
  | null x = Nothing
  | otherwise = Just $ foldr max (head . toList $ x) x

-- 6.
null' :: (Foldable t) => t a -> Bool
null' x = 0 == getSum (foldMap (\_ -> Sum 1) x)

-- 7.
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

-- 8.
toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (: [])

-- 9.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (<> mempty)

-- 10.
foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (\v acc -> f v <> acc) mempty