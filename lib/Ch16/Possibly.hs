module Ch16.Possibly where

import Test.QuickCheck

fi :: (Functor f, Eq (f a)) => f a -> Bool
fi f = fmap id f == f

fc :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
fc (Fun _ f) (Fun _ g) x = fmap g (fmap f x) == fmap (g . f) x

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = oneof [return LolNope, Yeppers <$> arbitrary]

test :: IO ()
test = do
  quickCheck (fi :: Possibly Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Float -> Possibly Int -> Bool)