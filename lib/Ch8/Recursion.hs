module Ch8.Recursion where

-- 1. pass

-- 2.
mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
mySum n = n + mySum (n - 1)

-- 3.
myMul :: (Integral a) => a -> a -> a
myMul 0 _ = 0
myMul _ 0 = 0
myMul 1 b = b
myMul a 1 = a
myMul a b = a + myMul a (b - 1)
