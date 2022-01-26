{-# LANGUAGE InstanceSigs #-}

module Ch23.WriteStateForYourself where

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let (a, _) = g s
     in (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) = Moi $ \s ->
    let (a, _) = g s
        (f', _) = f s
     in (f' a, s)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) g = Moi $ \s ->
    let (a, _) = f s
        (Moi sb) = g a
        (b, _) = sb s
     in (b, s)

main :: IO ()
main = do
  let moi :: Moi Int Int
      moi = Moi $ \s -> (0, s)
      f = (+ 1) <$> moi
  print $ runMoi f 0
