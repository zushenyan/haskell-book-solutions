module Ch12.ItsOnlyNatural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ go x
  where
    go :: Integer -> Nat
    go 0 = Zero
    go y = Succ $ go (y - 1)

test :: IO ()
test = do
  print $ natToInteger Zero
  print $ natToInteger (Succ Zero)
  print $ natToInteger (Succ (Succ Zero))
  print $ integerToNat 0
  print $ integerToNat 1
  print $ integerToNat 2
  print $ integerToNat (-1)