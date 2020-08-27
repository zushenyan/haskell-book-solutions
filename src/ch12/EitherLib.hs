module EitherLib where

-- 1.
sampleLefts' = [Left 1, Left 2, Left 3]

lefts' :: [Either a b] -> [a]
lefts' = foldr fPred []
 where
  fPred :: Either a b -> [a] -> [a]
  fPred (Left x) acc = x : acc
  fPred _        acc = acc

-- 2.
sampleRights' = [Right 1, Right 2, Right 3]

rights' :: [Either a b] -> [b]
rights' = foldr fPred []
 where
  fPred :: Either a b -> [b] -> [b]
  fPred (Right x) acc = x : acc
  fPred _         acc = acc

-- 3.
samplePartitionEithers' = [Left 1, Right 2, Left 3, Right 4]

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr fPred ([], [])
 where
  fPred :: Either a b -> ([a], [b]) -> ([a], [b])
  fPred (Left  x) (y, z) = (x : y, z)
  fPred (Right x) (y, z) = (y, x : z)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left  _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  x) = f x
either' _ g (Right x) = g x

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left  _) = Nothing
eitherMaybe'' f (Right x) = Just $ f x
