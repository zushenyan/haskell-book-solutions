module Ch12.ValidateTheWord where

import Data.List (partition)

newtype Word' = Word' String deriving (Eq, Show)

vowels :: [Char]
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = x `elem` vowels

mkWord :: String -> Maybe Word'
mkWord str = go . partitionByVowel $ str
  where
    partitionByVowel = partition isVowel
    go :: (String, String) -> Maybe Word'
    go (v, c)
      | length v <= length c = Just $ Word' str
      | otherwise = Nothing
