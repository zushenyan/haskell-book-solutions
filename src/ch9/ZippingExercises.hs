module ZippingExercises where

{-
1. Write your own version of zip, and ensure it behaves the same as the original:
zip :: [a] -> [b] -> [(a, b)]
zip = undefined
-}

zip' :: [a] -> [b] -> [(a, b)]
zip' []       _        = []
zip' _        []       = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

{-
2. Do what you did for zip but now for zipWith:
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined
-}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f []       _        = []
zipWith' f _        []       = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

{-
3. Rewrite your zip in terms of the zipWith you wrote.
-}
zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)
