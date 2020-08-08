module ElectricTypealoo where

{-
  1. chk :: Eq b => (a -> b) -> a -> b -> Bool chk = ???
-}
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

{-
  2. Hint: use some arithmetic operation to combine values of type b. Pick one:
-}
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromInteger i
