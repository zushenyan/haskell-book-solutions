module Ch12.WriteYourOwnIterateAndUnfoldr where

-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = f a : myIterate f (f a)

test1 :: IO ()
test1 = do
  print $ take 10 $ myIterate (+ 1) 0

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  Just (x, y) -> x : myUnfoldr f y

test2 :: IO ()
test2 = do
  print $ myUnfoldr (\x -> if x == 5 then Nothing else Just (x, x + 1)) 0
  print $ take 10 $ myUnfoldr (\x -> Just (x, x + 1)) 0

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\v -> Just (v, f v))

test3 :: IO ()
test3 = do
  print $ take 10 $ betterIterate (+ 1) 0