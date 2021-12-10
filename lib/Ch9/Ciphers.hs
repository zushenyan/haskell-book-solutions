module Ch9.Ciphers where

import Data.Char (chr, ord)

shift :: Int -> Char -> Char
shift i c = chr $ mod (orgChar + i) alphabetLength + start
  where
    start = ord 'a'
    orgChar = ord c - start
    alphabetLength = 26

shift5 :: Char -> Char
shift5 = shift 5

unshift5 :: Char -> Char
unshift5 = shift (-5)

caesar :: String -> String
caesar = map shift5

unCaesar :: String -> String
unCaesar = map unshift5