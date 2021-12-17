module Ch11.AsPatterns where

import Data.Char (toUpper)

-- 1.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf [] _ = True
isSubseqOf (x : xs) y = x `elem` y && isSubseqOf xs (tail . dropWhile (/= x) $ y)

isSubseqOf' :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf' [] _ = True
isSubseqOf' _ [] = False
isSubseqOf' xt@(x : xs) (y : ys) = if x == y then isSubseqOf' xs ys else isSubseqOf' xt ys

testIsSubseqOf :: IO ()
testIsSubseqOf = do
  print $ isSubseqOf' "blah" "blahwoot"
  print $ isSubseqOf' "blah" "wootblah"
  print $ isSubseqOf' "blah" "wboloath"
  print $ isSubseqOf' "blah" "tootbla"
  print $ isSubseqOf' "blah" "halbwoot"
  print $ isSubseqOf' "blah" "blawhoot"

-- 2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map trans . words
  where
    trans :: String -> (String, String)
    trans [] = ("", "")
    trans xt@(x : xs) = (xt, toUpper x : xs)

testCapitalizeWords :: IO ()
testCapitalizeWords = do
  print $ capitalizeWords "hello world" == [("hello", "Hello"), ("world", "World")]
