module Ch11.HuttonsRazor where

-- 1.
data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b

test1 :: IO ()
test1 = do
  print $ eval (Add (Lit 1) (Lit 9001))

-- 2.
printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

test2 :: IO ()
test2 = do
  print $ printExpr a1
  print $ printExpr a2
  print $ printExpr a3
  where
    a1 = Add (Lit 9001) (Lit 1)
    a2 = Add a1 (Lit 20001)
    a3 = Add (Lit 1) a2
