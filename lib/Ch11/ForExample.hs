module Ch11.ForExample where

data Example = MakeExample deriving (Show)

-- 1.
-- Example, "error: Data constructor not in scope: Example"

-- 2.
-- type Example :: *
-- data Example = MakeExample

-- 3.
data Example' = MakeExample' Int deriving (Show)

-- MakeExample' :: Int -> Example'