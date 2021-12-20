module Ch12.StringProcessing where

import Data.Maybe (fromMaybe)

-- 1.
notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise = Just x

replaceThe :: String -> String
replaceThe = unwords . map replace . words
  where
    replace :: String -> String
    replace x = fromMaybe "a" (notThe x)

test1 :: IO ()
test1 = do
  print $ replaceThe "the cow loves us"

-- 2.
vowels :: String
vowels = "aeiou"

hasVowel :: String -> Bool
hasVowel "" = False
hasVowel (x : _) = x `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (words str)
  where
    go :: [String] -> Integer
    go [] = 0
    go (x : y) = if x == "the" && hasVowel (head y) then 1 + go y else go y

test2 :: IO ()
test2 = do
  print $ countTheBeforeVowel "the cow"
  print $ countTheBeforeVowel "the evil cow"

-- 3.
countVowels :: String -> Integer
countVowels = foldr (\v acc -> if v `elem` vowels then acc + 1 else acc) 0

test3 :: IO ()
test3 = do
  print $ countVowels "the cow"
  print $ countVowels "Mikolajczak"