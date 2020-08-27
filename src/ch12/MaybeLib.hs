module MaybeLib where

-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing  = x
mayybee x f (Just y) = f y

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe x (Just y) = y

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe []       = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr fPred []
 where
  fPred :: Maybe a -> [a] -> [a]
  fPred Nothing  acc = acc
  fPred (Just a) acc = a : acc

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe x | hasNothing x = Nothing
            | otherwise    = Just $ catMaybes x
 where
  fPred :: Maybe a -> Bool -> Bool
  fPred Nothing  _   = True
  fPred (Just _) acc = acc
  hasNothing :: [Maybe a] -> Bool
  hasNothing = foldr fPred False
