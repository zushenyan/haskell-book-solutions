module HeavyLifting where

-- 1.
a :: [Int]
a = (+ 1) <$> read "[1]"

run1 :: IO ()
run1 = print a

-- 2.
b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

run2 :: IO ()
run2 = print b

-- 3.
c :: (Num a) => a -> a
c = (* 2) . subtract 2

run3 :: IO ()
run3 = print $ c 1

-- 4.
d :: Int -> String
d = (return '1' ++) . show . (\x -> [x, 1 .. 3])

run4 :: IO ()
run4 = print $ d 0

-- 5.
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = read . ("123" ++) . show <$> ioi
   in (* 3) <$> changed

run5 :: IO Integer
run5 = e