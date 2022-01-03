module Ch18.ChapterExercises2 where

import Control.Monad

-- 1.
j :: Monad m => m (m a) -> m a
j = join

run1 :: IO ()
run1 = do
  print $ j [[1, 2], [], [3]]
  print $ j (Just (Just 1))
  print $ j (Just Nothing :: Maybe (Maybe Int))
  print $ j (Nothing :: Maybe (Maybe Int))

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh ma f = meh (f <$> ma) id

-- t a -> (a -> m b) -> m t b
-- a = f <$> ma :: (a -> m b) -> t a -> t (m b)
-- meh a id :: t m a -> (m a -> m a) -> m (t a)
-- meh ma f = sequence $ f <$> ma

-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType ma = meh ma id