module Ch20.LibraryFunctions where

import Data.Foldable
import Data.Maybe
import Data.Monoid

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = getSum . foldMap Sum

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

product'' :: (Foldable t, Num a) => t a -> a
product'' = getProduct . foldMap Product

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = foldr (\v acc -> v == a || acc) False

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' a = getAny . foldMap (\v -> Any (a == v))

-- 4.
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' ta
  | null ta = Nothing
  | otherwise =
    let xa = toList ta
        init = head xa
     in Just . foldr min init $ xa

-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' ta
  | null ta = Nothing
  | otherwise =
    let xa = toList ta
        init = head xa
     in Just . foldr max init $ xa

-- 6.
null' :: (Foldable t) => t a -> Bool
null' = (== 0) . foldr (\_ acc -> acc + 1) 0

-- 7.
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0

-- 8.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (<> mempty)

-- 10.
myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr (\v acc -> f v <> acc) mempty