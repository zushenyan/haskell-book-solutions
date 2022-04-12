{-# LANGUAGE InstanceSigs #-}

module Ch26.LiftMore where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- ReaderT
-- instance MonadTrans (ReaderT r)
--   where lift = ReaderT . const
--  const :: a -> b -> a
--  ReaderT :: (r -> m a) -> ReaderT r m a
--  ReaderT . const :: m a -> ReaderT r m a

-- 1.
newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a
  lift = EitherT . liftM Right

-- 2.
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  -- lift ma = StateT $ \s -> do
  --   a <- ma
  --   return (a, s)
  lift ma = StateT $ \s -> (,) <$> ma <*> pure s
