module Ch16.Either where

import Test.QuickCheck

fi :: (Functor f, Eq (f a)) => f a -> Bool
fi f = fmap id f == f

fc :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
fc (Fun _ f) (Fun _ g) x = fmap g (fmap f x) == fmap (g . f) x

-- 1.
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

test :: IO ()
test = do
  quickCheck (fi :: Sum Char Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> Sum Char Int -> Bool)

-- 2.
-- ans: because the kind for instance Functor is "* -> *", in order to align with the kind,
-- we have to use "instance Functor (Sum a)" and that means we will keep "a" as it is and change
-- the "b" only in "Sum a b".