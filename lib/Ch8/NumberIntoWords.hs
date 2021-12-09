module Ch8.NumberIntoWords where

import Data.List (intercalate, intersperse)

digitToWord :: Int -> String
digitToWord 0 = "Zero"
digitToWord 1 = "One"
digitToWord 2 = "Two"
digitToWord 3 = "Three"
digitToWord 4 = "Four"
digitToWord 5 = "Five"
digitToWord 6 = "Six"
digitToWord 7 = "Seven"
digitToWord 8 = "Eight"
digitToWord 9 = "Nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n = go (divMod10 n) []
  where
    divMod10 :: (Integral a) => a -> (a, a)
    divMod10 = flip divMod 10
    go :: (Integral a) => (a, a) -> [a] -> [a]
    go (0, a) arr = a : arr
    go (a, b) arr = go (divMod10 a) arr ++ (b : arr)

wordNumber :: Int -> String
wordNumber n = intercalate "-" . map digitToWord $ digits n
