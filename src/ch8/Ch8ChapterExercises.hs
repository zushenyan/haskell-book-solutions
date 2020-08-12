module Ch8ChapterExercises where
{-
1. Write out the steps for reducing dividedBy 15 2 to its final answer according to the Haskell code.
go 15 2 0
go 13 2 1
go 11 2 2
go 9 2 3
go 7 2 4
go 5 2 5
go 3 2 6
go 1 2 7
(7, 1)
-}
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
 where
  go n d count | n < d     = (count, n)
               | otherwise = go (n - d) d (count + 1)

{-
2. Write a function that recursively sums all numbers from 1 to n, n being the argument.
So if n is 5,you’d add 1+2+3+4+5 to get 15.The type should be (Eq a, Num a) => a -> a.
-}
mySum :: (Eq a, Num a) => a -> a
mySum n = go n 0
 where
  go n total | n == 0    = total
             | otherwise = go (n - 1) (total + n)

{-
3. Write a function that multiplies two integral numbers using recursive summation. 
The type should be (Integral a) => a -> a -> a.
-}
mul :: (Integral a) => a -> a -> a
mul x y = go x y 0
 where
  go x 1 total = total + x
  go x y total = go x (y - 1) (total + x)

{-
Fixing dividedBy
Our dividedBy function wasn’t quite ideal. For one thing, 
it is a partial function and doesn’t return a result (bottom) when given a divisor that is 0 or less.

Using the pre-existing div function, we can see how negative
numbers should be handled:
Prelude> div 10 2
5
Prelude> div 10 (-2)
-5
Prelude> div (-10) (-2)
5
Prelude> div (-10) (2)
-5

The next issue is how to handle zero. Zero is undefined for division in math, 
so we ought to use a datatype that lets us say there is no sensible result when the user divides by zero.
If you need inspiration, consider using the following datatype to handle this:
data DividedResult =
  Result Integer
  | DividedByZero
-}
data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy' :: (Num a, Ord a) => a -> a -> DividedResult
dividedBy' num 0 = DividedByZero
dividedBy' num denom =
  let
    num'   = abs num
    denom' = abs denom
    go n d count | n < d     = Result count
                 | otherwise = go (n - d) d (count + 1)
    recoverPostiveMinus (Result count)
      | (num < 0 && denom < 0) = Result count
      | (num < 0 || denom < 0) = Result (-count)
      | otherwise              = Result count
  in
    recoverPostiveMinus $ go num' denom' 0

{-
McCarthy 91 function
-}
mc91 :: (Ord a, Num a) => a -> a
mc91 x | x > 100   = x - 10
       | otherwise = mc91 . mc91 $ x + 11
