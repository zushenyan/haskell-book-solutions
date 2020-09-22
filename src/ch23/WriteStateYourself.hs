{-# LANGUAGE InstanceSigs #-}

module WriteStateYourself where

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let (a1, s1) = g s
     in (f a1, s1)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $
    \s ->
      let (a1, s1) = g s
          (fab, s2) = f s1
       in (fab a1, s2)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a1, s1) = f s
     in runMoi (g a1) s1