module Ch25.Exe where

import Control.Monad

newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving (Eq, Show)

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative f => Applicative (IdentityT f) where
  pure = IdentityT . pure
  (IdentityT f) <*> (IdentityT a) = IdentityT $ f <*> a

instance Monad m => Monad (IdentityT m) where
  return = pure

  -- (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
  (IdentityT ma) >>= f =
    let aimb = join (fmap runIdentityT (fmap f ma))
     in IdentityT aimb