module Ch10.ScanExercises where

fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs

-- 1.
fibs1 :: [Int]
fibs1 = take 20 fibs

-- 2.
fibs2 :: [Int]
fibs2 = takeWhile (< 100) fibs

-- 3.
factorial :: Int -> Int
factorial n = scanl (*) 1 [1 .. n] !! n