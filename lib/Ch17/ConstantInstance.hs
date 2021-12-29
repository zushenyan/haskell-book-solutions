module Ch17.ConstantInstance where

import Data.Monoid

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant (a <> b)

f :: Constant (Sum Integer) b
f = Constant $ Sum 1

g :: Constant (Sum Integer) b
g = Constant $ Sum 2

h1 :: Constant (Sum Integer) a -> Constant (Sum Integer) b
h1 = (f <*>)

h :: Constant (Sum Integer) b
h = h1 g

run :: IO ()
run = do
  print $ f <*> g
  print (pure 1 :: Constant String Int)