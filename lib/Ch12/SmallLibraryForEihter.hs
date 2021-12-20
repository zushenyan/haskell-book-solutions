module Ch12.SmallLibraryForEither where

-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr pred []
  where
    pred :: Either a b -> [a] -> [a]
    pred (Right _) acc = acc
    pred (Left a) acc = a : acc

test1 :: IO ()
test1 = do
  print $ lefts' [Left 1, Right 2, Left 3]

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr pred []
  where
    pred :: Either a b -> [b] -> [b]
    pred (Left _) acc = acc
    pred (Right b) acc = b : acc

test2 :: IO ()
test2 = do
  print $ rights' [Left 1, Right 2, Left 3]

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr pred ([], [])
  where
    pred :: Either a b -> ([a], [b]) -> ([a], [b])
    pred (Left a) (l, r) = (a : l, r)
    pred (Right b) (l, r) = (l, b : r)

test3 :: IO ()
test3 = do
  print $ partitionEithers' [Left 1, Right 2, Left 3, Right 4]

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right a) = Just . f $ a

test4 :: IO ()
test4 = do
  print $ eitherMaybe' (+ 1) (Left 1)
  print $ eitherMaybe' (+ 1) (Right 1)

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

test5 :: IO ()
test5 = do
  print $ either' (+ 1) (* 3) (Left 1)
  print $ either' (+ 1) (* 3) (Right 1)