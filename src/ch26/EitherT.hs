module EitherT where

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

-- 1.
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

-- 2.
instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure (pure x)
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

-- 3.
instance Monad m => Monad (EitherT e m) where
  return = pure
  v >>= f = EitherT $ do
    result <- runEitherT v
    case result of
      Left l -> return $ Left l
      Right r -> runEitherT $ f r

-- 4.
swapEither :: Either e a -> Either a e
swapEither (Left a) = Right a
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

-- 5.
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
-- eitherT f g (EitherT amb) = amb >>= either f g
eitherT f g (EitherT amb) = do
  v <- amb
  case v of
    Left a -> f a
    Right b -> g b