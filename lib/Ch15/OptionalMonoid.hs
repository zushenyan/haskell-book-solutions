module Ch15.OptionalMonoid where

import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only a) (Only b) = Only (a <> b)
  (<>) (Only a) _ = Only a
  (<>) _ (Only a) = Only a
  (<>) _ _ = Nada

instance Semigroup a => Monoid (Optional a) where
  mempty = Nada

main :: IO ()
main = do
  let onlySum = Only $ Sum 1
      onlyFour = Only $ Product 4
      onlyTwo = Only $ Product 2
  print $ onlySum `mappend` onlySum
  print $ onlyFour `mappend` onlyTwo
  print $ onlySum `mappend` Nada
  print $ Only [1] `mappend` Nada
  print $ Nada `mappend` onlySum
