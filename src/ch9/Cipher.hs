module Cipher where

import           Data.Char

shift :: Int -> Char -> Char
shift num char = chr newChar
 where
  ordA        = ord 'a'
  characters  = length ['a' .. 'z']
  shiftedChar = ord char - ordA + num
  newChar     = mod shiftedChar characters + ordA

ceaser :: Int -> String -> String
ceaser shiftNum = map (shift shiftNum)

unCeaser :: Int -> String -> String
unCeaser shiftNum = map (shift $ negate shiftNum)
