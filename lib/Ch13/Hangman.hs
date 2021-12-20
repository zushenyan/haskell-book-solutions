module Ch13.Hangman where

import System.IO (readFile)
import System.Random (randomRIO)

allWords :: IO [String]
allWords = do
  dict <- readFile "data/dict.txt"
  return $ lines dict

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO [String]
gameWords = do
  filter pred <$> allWords
  where
    pred x = lx >= minWordLength && lx < maxWordLength
      where
        lx = length x

randomWord :: [String] -> IO String
randomWord gw = do
  index <- randomRIO (0, length gw - 1)
  return $ gw !! index

randomWord' :: IO String
randomWord' = gameWords >>= randomWord