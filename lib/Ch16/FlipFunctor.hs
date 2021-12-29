{-# LANGUAGE FlexibleInstances #-}

module Ch16.FlipFunctor where

data Tuple a b = Tuple a b deriving (Eq, Show)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Tuple b) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

main :: IO ()
main = print $ (+ 1) <$> Flip (Tuple 1 "hello")