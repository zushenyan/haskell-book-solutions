{-# LANGUAGE InstanceSigs #-}

module GOTCHA where

import Control.Applicative (Applicative (liftA2))

newtype Compose f g a = Compose {getCompose :: f (g a)}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a

-- 1.
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- 2.
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative t => (a -> t b) -> Compose f g a -> t (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga
