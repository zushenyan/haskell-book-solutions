module Ch10.UnderstandingFolds where

-- 1.
-- ans: b, c

-- 2.
ans2 :: Int
ans2 = foldr (*) 1 [1 .. 3]

-- 3.
-- ans: a

-- 4.
-- ans: a

-- 5.
-- a.
a :: [Char]
a = foldr (++) [] ["woot", "WOOT", "woot"]

-- b.
b :: [Char]
b = foldr max [] ["feat", "is", "the", "little", "death"]

-- c.
c :: Bool
c = foldr (&&) True [False, True]

-- d.
d :: Bool
d = foldr (||) True [False, True]

-- e.
e :: [Char]
e = foldl (flip ((++) . show)) "" [1 .. 5]

-- f.
f :: String
f = foldr (\a b -> const (show a) b) "a" [1 .. 5]

-- g.
g :: Int
g = foldr (\a b -> const (fromEnum a) b) 0 "tacos"

-- h.
h :: Integer
h = foldl const 0 "burritos"

-- i.
i :: Char
i = foldl const 'z' [1 .. 5]