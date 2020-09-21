{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where

newtype Reader r a = Reader {runReader :: r -> a}

-- 1.
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

-- 2.
asks :: (r -> a) -> Reader r a
asks = Reader

-- 3.
instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r