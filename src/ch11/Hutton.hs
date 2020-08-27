module Hutton where

data Expr = Lit Integer | Add Expr Expr

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2

eval :: Expr -> Integer
eval (Add (Lit a) (Lit b)) = a + b

printExpr :: Expr -> String
printExpr (Lit a  ) = show a
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
