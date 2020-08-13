module EnumFromTo where

eft :: (Enum a, Ord a) => a -> a -> [a]
eft start stop | start == stop = [stop]
               | start < stop  = start : eft (succ start) stop
               | otherwise     = []

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
