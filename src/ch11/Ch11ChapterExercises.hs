{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ch11ChapterExercises where

import           Data.List
import           Data.List.Split
import           Data.Char

-- Ciphers
templateDecrypt = "MEET AT DAWN"
templateEncrypt = "MPPR AE OYWY"
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
vigCeaser k str = restoreSpaces str transformedStr
  where transformedStr = map (\(p, k) -> vigEncrypt p k) (zipKeyword str k)

vigDeceaser :: String -> String -> String
vigDeceaser k str = restoreSpaces str transformedStr
  where transformedStr = map (\(c, k) -> vigDecrypt c k) (zipKeyword str k)

-- As-patterns
-- 1.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf xt@(x : xs) (y : ys) =
  if x == y then isSubseqOf xs ys else isSubseqOf xt ys
-- isSubseqOf []       []      = True
-- isSubseqOf (x : _)  []      = False
-- isSubseqOf []       (y : _) = True
-- isSubseqOf (x : xs) y       = x `elem` y && isSubseqOf xs ys
--   where ys = dropWhile (/= x) y

testIsSubseqOf :: IO ()
testIsSubseqOf = do
  print $ isSubseqOf "blah" "blahwoot" -- True
  print $ isSubseqOf "blah" "wootblah" -- True
  print $ isSubseqOf "blah" "wboloath" -- True
  print $ isSubseqOf "blah" "wootbla" -- False
  print $ isSubseqOf "blah" "halbwoot" -- False
  print $ isSubseqOf "blah" "blawhoot" -- True
  print $ isSubseqOf "blah" "blah" -- True
  return ()

-- 2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map pred . words
 where
  pred :: String -> (String, String)
  pred []          = ("", "")
  pred xt@(x : xs) = (xt, toUpper x : xs)

testCapitalizeWords :: IO ()
testCapitalizeWords = do
  print
    $  capitalizeWords "hello world"
    == [("hello", "Hello"), ("world", "World")]
  return ()

-- Language exercises
-- 1.
capitalizeWord :: String -> String
capitalizeWord ""       = ""
capitalizeWord (x : xs) = toUpper x : xs

-- 2.
capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph x =
  intercalate ". " . map capitalizeWord . splitOn ". " $ x
