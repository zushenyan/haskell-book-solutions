module Ch5.WriteATypeSignature where

-- 1.
functionH :: [a] -> a
functionH (x:_) = x

-- 2.
functionC :: (Ord a, Ord b) => a -> b -> Bool
functionC x y = if (x > y) then True else False

-- 3.
functionS :: (a,b) -> b
functionS (x, y) = y
