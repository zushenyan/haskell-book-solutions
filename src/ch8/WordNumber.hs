module WordNumber where

import           Data.List                      ( intersperse )

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"

digits :: Int -> [Int]
digits n =
  let go :: Int -> [Int] -> [Int]
      go num result | num < 10  = num : result
                    | otherwise = go (num `div` 10) ((num `mod` 10) : result)
  in  go n []

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

