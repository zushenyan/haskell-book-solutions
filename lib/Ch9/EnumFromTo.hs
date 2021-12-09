module Ch9.EnumFromTo where

commonFunc :: (Ord a, Enum a) => a -> a -> [a]
commonFunc a b
  | a == b = [a]
  | a > b = []
  | otherwise = take (fromEnum b - fromEnum a + 1) (enumFrom a)

eftBool :: Bool -> Bool -> [Bool]
eftBool = commonFunc

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = commonFunc

eftInt :: Int -> Int -> [Int]
eftInt = commonFunc

eftChar :: Char -> Char -> [Char]
eftChar = commonFunc
