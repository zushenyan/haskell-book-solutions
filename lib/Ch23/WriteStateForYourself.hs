{-# LANGUAGE InstanceSigs #-}

module Ch23.WriteStateForYourself where

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- fmap f (Moi g) = Moi $ \s -> (f . fst . g $ s, s)
  fmap f (Moi g) = Moi $ \s ->
    case g s of
      (a, _) -> (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  -- (<*>) (Moi f) (Moi g) = Moi go
  --   where
  --     go s = (b, s)
  --       where
  --         a = fst $ g s
  --         ab = fst $ f s
  --         b = ab a
  (<*>) (Moi f) (Moi g) = Moi $ \s ->
    case g s of
      (a, _) -> case f s of
        (f', _) -> (f' a, s)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  -- (>>=) (Moi f) g = Moi go
  --   where
  --     go s = (b, s)
  --       where
  --         a = fst $ f s
  --         sb = runMoi (g a) s
  --         b = fst sb
  (>>=) (Moi f) g = Moi $ \s ->
    case f s of
      (a, _) -> case g a of
        (Moi sb) -> case sb s of
          (b, _) -> (b, s)

main :: IO ()
main = do
  let moi :: Moi Int Int
      moi = Moi $ \s -> (0, s)
      f = (+ 1) <$> moi
  print $ runMoi f 0