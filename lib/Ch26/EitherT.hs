{-# LANGUAGE InstanceSigs #-}

module Ch26.EitherT where

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

-- 1.
instance (Functor m) => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

-- 2.
instance (Applicative m) => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (<*>) (EitherT f) (EitherT ema) = EitherT $ (<*>) <$> f <*> ema

-- 3.
instance (Monad m) => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (>>=) (EitherT ema) f = EitherT $ do
    v <- ema
    case v of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

-- 4.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema
  where
    swapEither :: Either e a -> Either a e
    swapEither (Left e) = Right e
    swapEither (Right a) = Left a

-- 5.
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT amb) = do
  v <- amb
  case v of
    Left a -> fa a
    Right b -> fb b