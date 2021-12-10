module Ch9.ZippingExercises where

-- 1.
myZip :: [a] -> [b] -> [(a, b)]
myZip a b = go a b []
  where
    go [] y arr = arr
    go x [] arr = arr
    go (x : xs) (y : ys) arr = (x, y) : arr ++ go xs ys arr

-- 2.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = go a b []
  where
    go [] y arr = arr
    go x [] arr = arr
    go (x : xs) (y : ys) arr = f x y : arr ++ go xs ys arr

-- 3.
myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)