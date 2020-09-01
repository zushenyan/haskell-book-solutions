module Iterate where

import           Data.List

-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : myIterate f (f n)

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go (f b)
 where
  go Nothing       = []
  go (Just (x, y)) = x : go (f y)

f :: Int -> Maybe (Int, Int)
f 5 = Nothing
f x = Just (x, x + 1)

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr f' where f' x' = Just (x', f x')
