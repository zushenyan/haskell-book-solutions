module StateT where

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

-- 1.
instance (Functor m) => Functor (StateT s m) where
  fmap f m = StateT $ \s ->
    (fmap (\(a, s') -> (f a, s')) . runStateT m) s

-- 2.
instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  StateT mf <*> StateT ma = StateT $ \s -> do
    (f, s') <- mf s
    (x, s'') <- ma s'
    return (f x, s'')

-- 3.
instance (Monad m) => Monad (StateT s m) where
  return = pure
  ma >>= f = StateT $ \s -> do
    (a, s') <- runStateT ma s
    runStateT (f a) s'
