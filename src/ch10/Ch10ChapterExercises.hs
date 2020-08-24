module Ch10ChapterExercises where

import           Data.List

{- Warm up -}
stops = "pbtdkg"
vowels = "aeiou"
{-
1.a 
Write a function that takes inputs from stops and vowels and makes 3-tuples of all
possible stop-vowel-stop combinations.
These will not all correspond to real words in English,
although the stop-vowel-stop pattern is common enough that many of them will.
-}
exe1a = [ (x, y, z) | x <- stops, y <- vowels, z <- stops ]

{-
1.b
Modify that function so that it only returns the combinations that begin with a p.
-}
exe1b = [ (x, y, z) | x <- stops, x == 'p', y <- vowels, z <- stops ]

{-
1.c
Now set up lists of nouns and verbs (instead of stops and vowels),
and modify the function to make tuples represent- ing possible noun-verb-noun sentences.
-}
exe1c = [ (x, y, z) | x <- nouns, y <- verbs, z <- nouns ]
 where
  nouns = ["dog", "cat", "human"]
  verbs = ["likes", "is", "gets"]

{-
2. What does the following mystery function do? What is its type?
Try to get a good sense of what it does before you test it in the REPL to verify it:
-}
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))
-- get how average characters in each words

{-
3. We’d really like the answer to be more precise. Can you rewrite that using fractional division?
-}
seekritFunc' :: String -> Double
seekritFunc' x = fromIntegral totalValidCharacters / fromIntegral wordCounts
 where
  str                  = words x
  totalValidCharacters = sum . map length $ str
  wordCounts           = length str

{- Rewriting -}
-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\v result -> f v || result) False

-- 3. Write two versions of myElem. One version should use folding and the other should use any:
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\v result -> x == v || result) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

-- 4. Implement myReverse. Don’t worry about trying to make it lazy:
myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

-- 5. Write myMap in terms of foldr. It should have the same behavior as the built-in map:
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x arr -> f x : arr) []

-- 6. Write myFilter in terms of foldr. It should have the same behavior as the built-in filter:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x arr -> if f x then x : arr else arr) []

-- 7. squish flattens a list of lists into a list:
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap maps a function over a list and concatenates the result:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x arr -> f x ++ arr) []

-- 9. squishAgain flattens a list of lists into a list. This time, re-use the squishMap function:
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy takes a comparison function and a list and
-- returns the greatest element of the list based on the last value that the comparison returns GT for:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f arr = foldr comp (head arr) arr
 where
  comp a b = case f a b of
    EQ -> a
    GT -> a
    LT -> b

--  11. myMinimumBy takes a comparison function and a list and
-- returns the least element of the list based on the last value that the comparison returns LT for:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f arr = foldr comp (head arr) arr
 where
  comp a b = case f a b of
    EQ -> a
    GT -> b
    LT -> a
