module Ch5.DoesItCompile where

-- 1.
bigNum :: (Integral b, Num a) => b -> a
bigNum = (^) 5
wahoo :: Num a => a
wahoo = bigNum 10

-- 2.
x :: [Char] -> IO ()
x = print
y :: IO ()
y = print "woohoo!"
z :: IO ()
z = x "hello world"

-- 3.
a :: Integer -> Integer -> Integer
a = (+)
b :: Integer -> Integer -> Integer
b = a
c :: Integer -> Integer
c = b 10
d :: Integer
d = c 200

-- 4.
a' :: Integer
a' = 12 + b'
b' :: Integer
b' = 10000 * c'
c' :: Integer
c' = 1