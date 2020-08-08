module GrabBag where

{-
  3. Next, we’ll practice writing anonymous lambda syntax. For example, one could rewrite:
    addOne x = x + 1
    Into:
    addOne = \x -> x + 1
-}
{-
  a) Rewrite the f function in the where clause:
    addOneIfOdd n = 
      case odd n of
        True -> f n
        False -> n
      where f n = n + 1
-}
addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = \n -> n + 1


{-
  b) Rewrite the following to use anonymous lambda syntax:
  addFive x y = (if x > y then y else x) + 5
-}
addFive = \x -> \y -> (if x > y then y else x) + 5

{-
  c) Rewrite the following so that it doesn’t use anonymous
  lambda syntax:
  mflip f = \x -> \y -> f y x
-}
mflip f x y = f y x
