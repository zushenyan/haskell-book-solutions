{-# LANGUAGE InstanceSigs #-}

module Bifunctor where

import Data.Bifunctor

-- 1.
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

-- 2.
data Const a b = Const a

instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)

-- 3.
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4.
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

-- 5.
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a

-- 7.
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 8.
data MyEither a b = MyLeft a | MyRight b

instance Bifunctor MyEither where
  bimap f g (MyLeft a) = MyLeft (f a)
  bimap f g (MyRight b) = MyRight (g b)
