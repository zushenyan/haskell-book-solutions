module Ch7.GrabBag where

-- 1.
-- ans: a,b,c,d

-- 2.
-- ans: d

-- 3.
-- a.
addOneIfOdd :: Integral p => p -> p
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where
    f = \n -> n + 1

-- b.
addFive :: Integer -> Integer -> Integer
addFive = \x y -> (if x > y then y else x) + 5

-- c.
mflip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
mflip f x y = f y x