module Ch13.HangmanGameLogic where

import Ch12.SmallLibraryForMaybe (catMaybes)
import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.List (intersect, intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, readFile, stdout)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter pred aw)
  where
    pred x = lx >= minWordLength && lx < maxWordLength
      where
        lx = length x

randomWord :: WordList -> IO String
randomWord (WordList gw) = do
  index <- randomRIO (0, length gw - 1)
  return $ gw !! index

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = intersperse ' ' (fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed
    where
      renderPuzzleChar :: Maybe Char -> Char
      renderPuzzleChar Nothing = '_'
      renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map (const Nothing) str) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) c = c `elem` str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ str) c = c `elem` str

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where
    zipper :: Char -> Char -> Maybe Char -> Maybe Char
    zipper guessed wordChar guessChar = if wordChar == guessed then Just wordChar else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledInSoFar guessed) = when (length guessed > length wordToGuess) $ do
  putStrLn "Your lose!"
  putStrLn $ "The word was: " ++ wordToGuess
  exitSuccess
  where
    filledInSoFar' = catMaybes filledInSoFar
    failedTimes = length guessed - length (filledInSoFar' `intersect` guessed)

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = when (all isJust filledInSoFar) $ do
  putStrLn "You win!"
  exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle