module SomeInstances where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

-- 1.
instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ fmap (fmap f) m

instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ pure (pure a)
  MaybeT f <*> MaybeT a = MaybeT $ (<*>) <$> f <*> a

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  MaybeT ma >>= f = MaybeT $ do
    result <- ma
    case result of
      Nothing -> return Nothing
      Just a -> runMaybeT $ f a

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- 2.
instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance Monad m => Applicative (ReaderT r m) where
  pure a = ReaderT $ \_ -> pure a
  ReaderT mf <*> ReaderT ma = ReaderT $ \r -> do
    f <- mf r
    a <- ma r
    pure $ f a

instance Monad m => Monad (ReaderT r m) where
  return = pure
  ma >>= f = ReaderT $ \r -> do
    a <- runReaderT ma r
    runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift ma = ReaderT $ const ma

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

-- 3.
instance (Functor m) => Functor (StateT s m) where
  fmap f m = StateT $ \s ->
    (fmap (\(a, s') -> (f a, s')) . runStateT m) s

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  StateT mf <*> StateT ma = StateT $ \s -> do
    (f, s') <- mf s
    (x, s'') <- ma s'
    return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  ma >>= f = StateT $ \s -> do
    (a, s') <- runStateT ma s
    runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO