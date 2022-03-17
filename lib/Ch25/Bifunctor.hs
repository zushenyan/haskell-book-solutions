module Ch25.Bifunctor where

import Data.Bifunctor

-- .1
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

-- 2.
newtype Const a b = Const a

instance Bifunctor Const where
  bimap f g (Const a) = Const $ f a

-- 3.
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4.
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a $ f b

-- 5.
newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a

-- 6.
data Quadriceps a b c d = Quadriceps a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadriceps a b c d) = Quadriceps a b (f c) (g d)

-- 7.
data MyEither a b = MyLeft a | MyRight b

instance Bifunctor MyEither where
  bimap f _ (MyLeft a) = MyLeft $ f a
  bimap _ g (MyRight b) = MyRight $ g b
