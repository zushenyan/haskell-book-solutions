module Ch11.Ciphers2 where

import Data.Char (chr, isSpace, ord)
import Data.List (elemIndices)

templateDecrypt :: [Char]
templateDecrypt = "MEET AT DAWN"

templateEncrypt :: [Char]
templateEncrypt = "MPPR AE OYWY"

templateKeyword :: [Char]
templateKeyword = "ALLY"

zipKeyword :: String -> String -> [(Char, Char)]
zipKeyword str keyword = zip (filter (not . isSpace) str) (cycle keyword)

vigEncrypt :: Char -> Char -> Char
vigEncrypt p k = chr . (+ ord 'A') $ (ord p + ord k) `mod` 26

vigDecrypt :: Char -> Char -> Char
vigDecrypt c k = chr . (+ ord 'A') $ (ord c - ord k) `mod` 26

insertAt :: Int -> a -> [a] -> [a]
insertAt i x arr = let (ys, zs) = splitAt i arr in ys ++ [x] ++ zs

insertSpaces :: [Int] -> String -> String
insertSpaces indices str = foldl (\acc i -> insertAt i ' ' acc) str indices

restoreSpaces :: String -> String -> String
restoreSpaces originalStr = insertSpaces (elemIndices ' ' originalStr)

vigCeaser :: String -> String -> String
vigCeaser [] str = str
vigCeaser k str = restoreSpaces str transformedStr
  where
    transformedStr = map (uncurry vigEncrypt) (zipKeyword str k)

vigDeceaser :: String -> String -> String
vigDeceaser [] str = str
vigDeceaser k str = restoreSpaces str transformedStr
  where
    transformedStr = map (uncurry vigDecrypt) (zipKeyword str k)