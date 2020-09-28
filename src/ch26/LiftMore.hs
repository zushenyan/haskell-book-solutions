module LiftMore where

import Control.Monad.Trans (MonadTrans (..))

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

--  1.
instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

-- 2.
instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)