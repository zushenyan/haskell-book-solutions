module Ch12.FinallySomethingOtherThanAList where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (x, y, z) -> Node (unfold f x) y (unfold f z)

test1 :: IO ()
test1 = do
  print $ unfold (\x -> if x <= 3 then Just (x + 1, x, x + 1) else Nothing) 0

-- 2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold (\v -> if v == x then Nothing else Just (v + 1, v, v + 1)) 0

test2 :: IO ()
test2 = do
  print $ treeBuild 0
  print $ treeBuild 1
  print $ treeBuild 2
  print $ treeBuild 3