{-# LANGUAGE InstanceSigs #-}

module Ch26.StateT where

import Control.Applicative
import Data.Tuple

applyFst :: (a -> c) -> (a, b) -> (c, b)
applyFst f = swap . fmap f . swap

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  -- fmap f (StateT sma) = StateT $ fmap (applyFst f) . sma
  fmap f (StateT sma) = StateT $ (fmap . fmap) (applyFst f) sma

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateT smab) (StateT sma) = StateT $ \s -> do
    abs <- smab s
    as <- sma s
    let f = fst abs
        r = applyFst f as
    return r

instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateT sma) f = StateT $ \s -> do
    as <- sma s
    runStateT (f . fst $ as) s