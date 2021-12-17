module Ch11.LanguageExercises where

import Data.Char (isAlpha, toUpper)
import Data.List (intercalate)

-- 1.
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs) = toUpper x : xs

test1 :: IO ()
test1 = do
  print $ capitalizeWord "Chortle" == "Chortle"
  print $ capitalizeWord "chortle" == "Chortle"

-- 2.
toUpperFirstLetter :: String -> String
toUpperFirstLetter str = front ++ capitalizeWord back
  where
    front = takeWhile (not . isAlpha) str
    back = dropWhile (not . isAlpha) str

splitBy :: Char -> String -> [String]
splitBy delimeter str = go (breakByDelimeter str)
  where
    breakByDelimeter = break (== delimeter)
    go :: (String, String) -> [String]
    go (x, []) = [x]
    go (x, [delimeter]) = [x, ""]
    go (x, _ : ys) = x : go (breakByDelimeter ys)

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate "." . map toUpperFirstLetter . splitBy '.'

sample :: [Char]
sample = "blah. woot ha."

test2 :: IO ()
test2 = do
  print $ capitalizeParagraph sample == "Blah. Woot ha."