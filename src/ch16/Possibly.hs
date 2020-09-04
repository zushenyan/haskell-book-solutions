module Possibly where

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

run :: IO ()
run = do
  print $ (show <$> Yeppers 123) == Yeppers "123"
  print $ ((+ 1) <$> Yeppers 1) == Yeppers 2
  print $ ((+ 1) <$> LolNope) == LolNope
