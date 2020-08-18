module Ch9Exercises where

import           Data.Char

-- Data.Char

{-
2. Given the following behaviors, which would we use to write a function that filters all the uppercase letters out of a String? Write that function such that,
given the input "HbEfLrLxO", your function will return "HELLO".
-}

myFilter :: String -> String
myFilter = filter isUpper

{-
3. Write a function that will capitalize the first letter of a string and return the entire string.
For example, if given the argument "julie", it will return "Julie".
-}
myCapital :: String -> String
myCapital (x : xs) = toUpper x : xs

{-
4. Now make a new version of that function that is recursive, such that if you give it the input "woot", it will holler back at you "WOOT".
The type signature won’t change, but you will want to add a base case.
-}
myCapitalRec :: String -> String
myCapitalRec ""       = ""
myCapitalRec (x : xs) = toUpper x : myCapitalRec xs

{-
5. To do the final exercise in this section, we’ll need another standard function for lists called head.
Query the type of head, and experiment with it to see what it does.
Now write a function that will capitalize the first letter of a String and return only that letter as the result.

6. Cool. Good work. Now rewrite it as a composed function. Then, for fun, rewrite it point-free.
-}
myCapHead :: String -> Char
myCapHead = toUpper . head

-- Writing your own standard functions
{-
1. myOr returns True if any Bool in the list is True:
myOr :: [Bool] -> Bool
myOr = undefined
-}
myOr :: [Bool] -> Bool
myOr []       = False
myOr (x : xs) = x || myOr xs

{-
2. myAny returns True if a -> Bool applied to any of the values in the list returns True:

myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined

Example for validating myAny:
Prelude> myAny even [1, 3, 5]
False
Prelude> myAny odd [1, 3, 5]
True
-}
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f x  = foldr (\x' acc -> f x' || acc) False x

{-
3. After you write the recursive myElem, write another version that uses any.
The built-in version of elem in GHC 7.10 and newer has a type that uses Foldable instead of the list type, specifically.
You can ignore that and write the concrete version that works only for lists:

myElem :: Eq a => a -> [a] -> Bool

Prelude> myElem 1 [1..10]
True
Prelude> myElem 1 [2..10]
False
-}
myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

{-
4. Implement myReverse:
myReverse :: [a] -> [a]
myReverse = undefined

Prelude> myReverse "blah"
"halb"
Prelude> myReverse [1..5]
[5,4,3,2,1]
-}
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

{-
5. squish flattens a list of lists into a list:

squish :: [[a]] -> [a]
squish = undefined
-}
squish :: [[a]] -> [a]
squish = foldr (++) []

{-
6. squish Map maps a function over a list and concatenates the results:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined

Prelude> squishMap (\x -> [1, x, 3]) [2]
[1,2,3]
Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
"WO 1 HOO WO 2 HOO WO 3 HOO "
-}
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f x  = foldr (\x' acc -> f x' ++ acc) [] x

{-
7. squishAgain flattens a list of lists into a list. This time, re-use the squishMap function:
squishAgain :: [[a]] -> [a]
squishAgain = undefined
-}
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

{-
8. myMaximumBy takes a comparison function and a list and returns the greatest element of the list
based on the last value that the comparison returns GT for.
If you import maximumBy from Data.List, you’ll see that the type is:

Foldable t => (a -> a -> Ordering) -> t a -> a
Rather than:
(a -> a -> Ordering) -> [a] -> a

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined

Prelude> xs = [1, 53, 9001, 10]
Prelude> myMaximumBy compare xs
9001
-}
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f arr =
  foldr (\x acc -> if f x acc == GT then x else acc) (head arr) arr

{-
9. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returns LT for:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined

Prelude> xs = [1, 53, 9001, 10]
Prelude> myMinimumBy compare xs
1
-}
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f arr =
  foldr (\x acc -> if f x acc == LT then x else acc) (head arr) arr

{-
10. Using the myMinimumBy and myMaximumBy functions, 
write your own versions of maximum and minimum. 
If you have GHC 7.10 or newer, you’ll see a type constructor that wants a Foldable instance in- stead of a list,
as has been the case for many functions so far:

myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined
-}
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
