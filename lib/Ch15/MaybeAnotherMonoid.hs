module Ch15.MaybeAnotherMonoid where

import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [return Nada, return $ Only a]

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only a) (Only b) = Only (a <> b)
  (<>) (Only a) _ = Only a
  (<>) _ (Only a) = Only a
  (<>) _ _ = Nada

newtype First' a = First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

instance Semigroup (First' a) where
  (<>) (First' Nada) y = y
  (<>) x _ = x

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend a = (<> a)

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

run :: IO ()
run = do
  let onlyOne = First' $ Only 1
      onlyTwo = First' $ Only 2
      nada = First' Nada
  print $ onlyOne `mappend` nada
  print ((nada `mappend` nada) :: First' Int)
  print $ nada `mappend` onlyTwo
  print $ onlyOne `mappend` onlyTwo