module Ch7.GuardDuty where

-- 1.
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where
    y = x / 100

-- 2.
avgGrade' :: (Fractional a, Ord a) => a -> Char
avgGrade' x
  | y >= 0.7 = 'C'
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where
    y = x / 100

result :: Char
result = avgGrade' 90

-- 3.
-- ans: b

-- 4.
-- ans: String

-- 5.
pal :: String -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

-- 6.
-- ans: c

-- 7.
-- ans: (Num a, Ord a) => a

-- 8.
numbers :: (Num a, Ord a) => a -> Int
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | otherwise = 1