{-# LANGUAGE InstanceSigs #-}

module Ch25.Applicative where

import Control.Applicative

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- 1.
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- 2.
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: (Applicative h) => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga
