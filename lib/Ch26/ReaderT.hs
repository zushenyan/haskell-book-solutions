{-# LANGUAGE InstanceSigs #-}

module Ch26.ReaderT where

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  -- fmap f (ReaderT rma) = ReaderT $ fmap f . rma
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  -- pure a = ReaderT $ \r -> pure a
  pure = ReaderT . pure . pure

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  -- (<*>) (ReaderT rmab) (ReaderT rma) = ReaderT $ \r ->
  --   let mab = rmab r
  --       ma = rma r
  --    in mab <*> ma
  (<*>) (ReaderT rmab) (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  -- (>>=) (ReaderT rma) f = ReaderT $ \r ->
  --   let ma = rma r
  --       f' = flip (runReaderT . f) r
  --    in ma >>= f'
  (>>=) (ReaderT rma) f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r