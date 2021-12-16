module Ch11.DogTypes where

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- 1.
-- ans: type constructor

-- 2.
-- ans: * -> *

-- 3.
-- ans: *

-- 4.
-- ans: Num a => Doggies a

-- 5.
-- ans: Doggies Integer

-- 6.
-- ans: Doggies String

-- 7.
-- ans: both. Because they have the same name.

-- 8.
-- ans: doge -> DogueDeBordeaux doge

-- 9.
-- ans: DogueDeBordeaux String
