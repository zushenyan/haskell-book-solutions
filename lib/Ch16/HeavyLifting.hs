module Ch16.HeavyLifting where

-- 1.
a :: [Int]
a = (+ 1) <$> read "[1]"

-- 2.
b :: Maybe [[Char]]
b = fmap (++ "lol") <$> Just ["Hi,", "Hello"]

-- 3.
c :: Num c => c -> c
c = (* 2) . (\x -> x - 2)

-- 4.
d :: (Show a, Num a, Enum a) => a -> [Char]
d = (return '1' ++) . show . (\x -> [x, 1 .. 3])

-- 5.
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = read . ("123" ++) . show <$> ioi
   in (* 3) <$> changed