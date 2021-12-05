module Ch5.GivenATypeWriteTheFunction where

-- 1.
i :: a -> a
i a = a

-- 2.
c :: a -> b -> a
c a b = a

-- 3.
c'' :: b -> a -> b
c'' = c

-- 4.
c' :: a -> b -> b
c' a b = b

-- 5.
r :: [a] -> [a]
r a = a

-- 6.
co :: (b -> c) -> (a -> b) -> a -> c
co f g a = f $ g a

-- 7.
a :: (a -> c) -> a -> a
a _ b = b

-- 8.
a' :: (a -> b) -> a -> b
a' f = f
