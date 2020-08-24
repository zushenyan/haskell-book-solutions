module Fib where

fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs

{- 1. Modify your fibs function to only return the first 20 Fibonacci numbers. -}
fib1 :: [Int]
fib1 = take 20 fibs

{- 2. Modify fibs to return the Fibonacci numbers that are less than 100. -}
fib2 :: [Int]
fib2 = takeWhile (< 100) fibs

{-
3. Try to write the factorial function from Chapter 8 as a scan. You’ll want scanl again,
and your start value will be 1. 
Warning: this will also generate an infinite list,
so you may want to pass it through a take function or similar.
-}
fac :: [Int]
fac = scanl (*) 1 [2 ..]
