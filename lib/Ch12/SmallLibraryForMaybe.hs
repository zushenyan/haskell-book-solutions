module Ch12.SmallLibraryForMaybe where

-- 1.
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

test1 :: IO ()
test1 = do
  print $ isJust (Just 1)
  print $ isJust Nothing
  print $ isNothing (Just 1)
  print $ isNothing Nothing

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

test2 :: IO ()
test2 = do
  print $ mayybee 0 (+ 1) Nothing
  print $ mayybee 0 (+ 1) (Just 1)

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

test3 :: IO ()
test3 = do
  print $ fromMaybe 0 Nothing
  print $ fromMaybe 0 (Just 1)

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

test4 :: IO ()
test4 = do
  print $ listToMaybe [1, 2, 3]
  print $ listToMaybe ([] :: [Int])
  print $ maybeToList (Just 1)
  print (maybeToList Nothing :: [Int])

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr pred []
  where
    pred :: Maybe a -> [a] -> [a]
    pred Nothing acc = acc
    pred (Just a) acc = a : acc

test5 :: IO ()
test5 = do
  print $ catMaybes [Just 1, Nothing, Just 2]
  print (catMaybes (replicate 4 Nothing) :: [Int])

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr pred (Just [])
  where
    pred :: Maybe a -> Maybe [a] -> Maybe [a]
    pred Nothing _ = Nothing
    pred _ Nothing = Nothing
    pred (Just x) (Just xs) = Just (x : xs)

test6 :: IO ()
test6 = do
  print $ flipMaybe [Just 1, Just 2, Just 3]
  print $ flipMaybe [Just 1, Nothing, Just 3]