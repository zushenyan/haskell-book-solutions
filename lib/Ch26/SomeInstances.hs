{-# LANGUAGE InstanceSigs #-}

module Ch26.SomeInstances where

-- import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Tuple

-- 1.
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (<*>) (MaybeT f) (MaybeT ma) = MaybeT $ (<*>) <$> f <*> ma

instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) (MaybeT ma) f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      (Just a') -> runMaybeT $ f a'

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = lift . liftIO

-- 2.
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure = ReaderT . pure . pure

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (<*>) (ReaderT rmab) (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) (ReaderT rma) f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

-- 3.
applyFst :: (a -> c) -> (a, b) -> (c, b)
applyFst f = swap . fmap f . swap

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
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

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \s -> (,) <$> ma <*> pure s

instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: Monad m => IO a -> StateT s m a
  liftIO = lift . liftIO