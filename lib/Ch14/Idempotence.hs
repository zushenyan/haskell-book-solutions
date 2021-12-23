module Ch14.Idempotence where

import Ch11.LanguageExercises (capitalizeWord)
import Data.List (sort)
import Test.QuickCheck

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

-- 1.
f :: String -> Bool
f x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

test1 :: IO ()
test1 = quickCheck . property $ f

-- 2.
f' :: Ord a => [a] -> Bool
f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

test2 :: IO ()
test2 = quickCheck . property $ f