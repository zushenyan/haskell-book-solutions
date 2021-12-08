module Ch7.VarietyPack where

-- 1.
k :: (a, b) -> a
k (x, y) = x

k1 :: Integer
k1 = k ((4 -1), 10)

k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Integer
k3 = k (3, True)

-- a. see answers above
-- b. see answers above, no
-- c. k1 and k3

-- 2.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))