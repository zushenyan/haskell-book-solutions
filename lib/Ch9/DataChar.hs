module Ch9.DataChar where

import Data.Char (isUpper, toUpper)

-- 1. pass
-- 2.
fun2 :: String -> String
fun2 = filter isUpper

-- 3.
fun3 :: String -> String
fun3 [] = ""
fun3 (x : xs) = toUpper x : xs

-- 4.
fun4 :: String -> String
fun4 = foldr ((:) . toUpper) ""

-- 5 & 6.
fun5 :: String -> Char
fun5 = toUpper . head