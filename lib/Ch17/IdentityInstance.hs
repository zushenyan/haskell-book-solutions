module Ch17.IdentityInstance where

import Data.Monoid

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

f :: Identity (Sum Integer)
f = Identity $ Sum 1

g :: Identity (Sum Integer)
g = Identity $ Sum 2