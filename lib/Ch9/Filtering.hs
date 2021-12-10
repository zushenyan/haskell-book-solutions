module Ch9.Filtering where

-- 1.
func1 :: [Integer]
func1 = filter (\x -> rem x 3 == 0) [1 .. 30]

-- 2.
func2 :: Int
func2 = length func1

-- 3.
func3 :: String -> [String]
func3 = filter (\x -> x /= "the" && x /= "an" && x /= "a") . words