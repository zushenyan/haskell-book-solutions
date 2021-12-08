module Ch7.CasePractice where

-- 1.
functionC :: Ord p => p -> p -> p
functionC x y = case x > y of
  True -> x
  False -> y

-- 2.
ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

-- 3.
nums :: (Ord a, Num a, Num p) => a -> p
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0