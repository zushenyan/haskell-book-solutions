module Ch17.Checkers where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = oneof [return Fools, return Twoo]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = quickBatch $ monoid Twoo