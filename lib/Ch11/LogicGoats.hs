{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ch11.LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany a = a > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- 1.
-- newtype MyTuple = MyTuple (Int, String) deriving (Show, Eq)

instance TooMany (Int, String) where
  tooMany (a, _) = tooMany a

-- 2.
-- newtype GoatCounts = GoatCounts (Int, Int) deriving (Show, Eq)

instance TooMany (Int, Int) where
  tooMany (a, b) = tooMany $ a + b

-- 3.
newtype NumTooMany = NumTooMany (Int, Int) deriving (Show, Eq, TooMany)

--instance TooMany NumTooMany where
-- tooMany (NumTooMany (a, b)) = tooMany $ a * b