module Ch7ChapterExercises where
{-
  1. The following function returns the tens digit of an integral argument:
    tensDigit :: Integral a => a -> a
    tensDigit x = d
      where xLast = x `div` 10
      d = xLast `mod` 10
-}
{- a) First, rewrite it using divMod. -}
tensDigit :: Integral a => a -> a
tensDigit = fst . flip divMod 10

{- b) Does the divMod version have the same type as the original version? -}
-- yes

{-
  c) Next, let’s change it so that we’re getting the hundreds digit instead.
  You could start it like this (though that may not be the only possibility):
  hunsD x = d2
    where d = undefined
-}
hunsD :: Integral a => a -> a
hunsD = fst . flip divMod 100

{-
  2. Implement the following function of the type a -> a -> Bool -> a once using a case expression and once with a guard:
  foldBool :: a -> a -> Bool -> a
  foldBool = error "Error: Need to implement foldBool!"

  The result is semantically similar to if-then-else expressions but syntactically quite different.
  Here is the pattern matching version to get you started:
  foldBool3 :: a -> a -> Bool -> a
  foldBool3 x _ False = x
  foldBool3 _ y True = y
-}
foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
  False -> x
  True  -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool | not bool = x
                   | bool     = y

{-
  3. Fill in the definition. Note that the first argument to our function is also a function that can be applied to values.
    Your second argument is a tuple, which can be used for pattern matching:
    g :: (a -> b) -> (a, c) -> (b, c)
    g = undefined
-}
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

{-
  4. For this next exercise, you’ll experiment with writing point-free versions of existing code. This involves some new information, so read the following explanation carefully.
  Type classes are dispatched by type. Read is a type class like Show, but it is the dual or “opposite” of Show. In general, the Read type class isn’t something you should plan to use, but this exercise is structured to teach you something about the interaction between type classes and types.
  The function read in the Read type class has the type:

  read :: Read a => String -> a

  Notice a pattern?

  read :: Read a => String -> a
  show :: Show a => a -> String

  Type the following code into a source file. Then load it, and run it in GHCi to make sure you understand why the evaluation results in the answers you see:
  -- arith4.hs
  module Arith4 where

  roundTrip :: (Show a, Read a) => a -> a
  roundTrip a = read (show a)

  main = do
    print (roundTrip 4)
    print (id 4)
-}
{-
  5. Next, write a point-free version of roundTrip. 
  (n.b., this refers to the function definition, not to its application in main.)
-}
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

{-
  6. We will continue to use the code in module Arith4 for this exercise, as well.
  When we apply show to a value such as (1 :: Int), the a that implements Show is type Int,
  so GHC will use the Int instance of the Show type class to stringify our Int value 1.

  However, read expects a String argument in order to return an a. The String argument that is the first argument to read tells the function nothing about what type the de-stringified result should be. In the type signature roundTrip currently has, it knows, because the type variables are the same, so the type that is the input to show has to be the same type as the output of read.
  Your task now is to change the type of roundTrip in Arith4 to (Show a, Read b) => a -> b. How might we tell GHC which instance of Read to dispatch against the String? Make the expression print (roundTrip 4) work. You will only need the has the type syntax of :: and parentheses for scoping.
-}
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

test :: IO ()
test = do
  print (roundTrip' 4 :: Int)
  print (id 4)
