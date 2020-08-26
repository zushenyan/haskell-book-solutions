{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (> 42)

instance TooMany (Int, String) where
  tooMany (a, b) = tooMany a

-- instance TooMany (Int, Int) where
--   tooMany (a, b) = tooMany $ a + b

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany $ a + b

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype MyType = MyType (Int, String) deriving (Eq, Show, TooMany)
newtype MyType' = MyType' (Int, Int) deriving (Eq, Show, TooMany)
