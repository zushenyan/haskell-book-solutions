{-
  Exercises: Eq instances
  Write the Eq instances for the datatypes provided.
-}
module Eq where

{-
  1. It’s not a typo, we’re just being cute with the name:
     data TisAnInteger =
       TisAn Integer
-}
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'

{- 2. data TwoIntegers = Two Integer Integer -}
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two v1 v2) (Two v1' v2') = (v1 == v1') && (v2 == v2')

{- 3. data StringOrInt = TisAnInt Int | TisAString String -}
data StringOrInt = TisAnInt Int | TisAnString String

instance Eq StringOrInt where
  (==) (TisAnInt    v) (TisAnInt    v') = v == v'
  (==) (TisAnString v) (TisAnString v') = v == v'
  (==) _               _                = False

{- 4. data Pair a = Pair a a -}
data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  (==) (Pair v1 v2) (Pair v1' v2') = (v1 == v1') && (v2 == v2')

{- 5. data Tuple a b = Tuple a b -}
data Tuble a b = Tuble a b

instance (Eq a, Eq b) => Eq (Tuble a b) where
  (==) (Tuble v1 v2) (Tuble v1' v2') = (v1 == v1') && (v2 == v2')

{- 6. data Which a = ThisOne a | ThatOne a -}
data Which a = TheOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (TheOne  v) (TheOne  v') = v == v'
  (==) (ThatOne v) (ThatOne v') = v == v'
  (==) _           _            = False

{- 7. data EitherOr a b = Hello a | Goodbye b -}
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello   v) (Hello   v') = v == v'
  (==) (Goodbye v) (Goodbye v') = v == v'
  (==) _           _            = False
