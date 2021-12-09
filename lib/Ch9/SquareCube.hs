module Ch9.SquareCube where

mySqr :: [Integer]
mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube :: [Integer]
myCube = [y ^ 3 | y <- [1 .. 5]]

-- 1.
r1 :: [(Integer, Integer)]
r1 = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
r2 :: [(Integer, Integer)]
r2 = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

-- 3.
r3 :: Int
r3 = length r2