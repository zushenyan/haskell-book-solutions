module OtherThanList where

data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Show, Eq, Ord)

tree = Node (Node Leaf 1 (Node Leaf 2 Leaf))
            0
            (Node (Node Leaf 4 Leaf) 3 (Node Leaf 5 Leaf))

-- 1.
unfold :: (a -> Maybe (a, b, a)) -> a -> BTree b
unfold f a = go (f a)
 where
  go Nothing          = Leaf
  go (Just (x, y, z)) = Node (go $ f x) y (go $ f z)

f :: Int -> Maybe (Int, Int, Int)
f 2 = Nothing
f x = Just (x + 1, x, x + 1)

-- 2.
treeBuild :: Integer -> BTree Integer
treeBuild n = unfold f 0
 where
  f :: Integer -> Maybe (Integer, Integer, Integer)
  f x | x == n    = Nothing
      | otherwise = Just (x + 1, x, x + 1)

