module Ch5.Exercises where

-- 1.
f :: a -> a -> a -> a
f = undefined
x :: Char
x = undefined
r1 :: Char -> Char -> Char
r1 = f x

-- 2.
g :: a -> b -> c -> b
g = undefined
r2 :: Char
r2 = g 0 'c' "woot"

-- 3.
h :: (Num a, Num b) => a -> b -> b
h = undefined
r3 :: Integer
r3 = h 1.0 2

-- 4.
r4 :: Double
r4 = h 1 (5.5 :: Double)

-- 5.
jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined
r5 :: [Char]
r5 = jackal "keyboard" "has the word jackal in it"

-- 6.
r6 :: Eq b => b -> [Char]
r6 = jackal "keyboard"

-- 7.
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined
r7 :: Integer
r7 = kessel 1 2

-- 8.
r8 :: Integer
r8 = kessel 1 (2 :: Integer)

-- 9.
r9 :: Integer
r9 = kessel (1 :: Integer) 2