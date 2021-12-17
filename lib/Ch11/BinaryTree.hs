module Ch11.BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' a Leaf = Node Leaf a Leaf
insert' a (Node l b r)
  | a == b = Node l b r
  | a < b = Node (insert' a l) b r
  | otherwise = Node l b (insert' a r)

-- map
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l a r) = Node (mapTree f l) (f a) (mapTree f r)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup OK!"
    else error "test failed"

-- convert binanry tree to list
preorder :: BinaryTree a -> [a]
preorder tree = go tree []
  where
    go :: BinaryTree a -> [a] -> [a]
    go Leaf arr = arr
    go (Node l v r) arr = [v] ++ go l arr ++ go r arr

inorder :: BinaryTree a -> [a]
inorder tree = go tree []
  where
    go :: BinaryTree a -> [a] -> [a]
    go Leaf arr = arr
    go (Node l v r) arr = go l arr ++ [v] ++ go r arr

postorder :: BinaryTree a -> [a]
postorder tree = go tree []
  where
    go :: BinaryTree a -> [a] -> [a]
    go Leaf arr = arr
    go (Node l v r) arr = go l arr ++ go r arr ++ [v]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "postorder fine!"
    else putStrLn "bad news bears."

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder