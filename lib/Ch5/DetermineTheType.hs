{-# LANGUAGE NoMonomorphismRestriction #-}

module Ch5.DetermineTheType where

-- 1.
a :: Num a => a
a = (* 9) 6

b :: Num a => (a, [Char])
b = head [(0, "doge"), (1, "kitteh")]

c :: (Integer, [Char])
c  = head [(0 :: Integer, "doge"), (1, "kitteh")]

d :: Bool
d = if False then True else False

e :: Int
e = length [1,2,3,4,5]

f :: Bool
f = length [1,2,3,4] > length "TACOCAT"

-- 2.
x :: Num a => a
x = 5
y :: Num a => a
y = x + 5
w :: Num a => a
w = y * 10

-- 3.
x3 :: Num a => a
x3 = 5
y3 :: Num a => a
y3 = x + 5
z3 :: Num a => a -> a
z3 y = y * 10

-- 4.
x4 :: Num a => a
x4 = 5
y4 :: Num a => a
y4 = x + 5
f4 :: Fractional a => a
f4 = 4 / y4

-- 5.
x5 :: [Char]
x5 = "Julie"
y5 :: [Char]
y5 = " <3 "
z5 :: [Char]
z5 = "Haskell"
f5 :: [Char]
f5 = x5 ++ y5 ++ z5
