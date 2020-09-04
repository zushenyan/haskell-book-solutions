module ShortExercises where

-- 1.
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

run1 :: IO ()
run1 = do
  print $ ((+ 1) <$> First 1) == First 1
  print $ ((+ 1) <$> (Second 1 :: Sum Int Int)) == Second 2

-- 2.
-- Because Sum type has kind * -> * -> * but Functor requires it to be * -> *.