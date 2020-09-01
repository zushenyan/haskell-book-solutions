module Hangman where

import           Control.Monad                  ( forever
                                                , when
                                                )
import           Data.Char                      ( toLower )
import           Data.Maybe                     ( isJust )
import           Data.List                      ( intersperse
                                                , intersect
                                                )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           System.Random                  ( randomRIO )

newtype WordList = WordList [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList $ lines dict

-- gameWords :: IO WordList
-- gameWords = do
--   aw <- allWords
--   return $ filter wordLength aw
--  where
--   wordLength w = let l = length w in l >= minWordLength && l <= maxWordLength

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return . WordList $ filter wordLength aw
 where
  wordLength w = let l = length w in l >= minWordLength && l <= maxWordLength

randomWords :: WordList -> IO String
randomWords (WordList wl) = do
  index <- randomRIO (0, length wl)
  return $ wl !! index

randomWords' :: IO String
randomWords' = gameWords >>= randomWords

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
      ++ " Gusssed so far: "
      ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w mw [] where mw = map (const Nothing) w

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word fillInSoFar s) c = Puzzle word
                                                       newFillInSoFar
                                                       (c : s)
 where
  zipper wordChar guessChar =
    if wordChar == c then Just wordChar else guessChar
  newFillInSoFar = zipWith zipper word fillInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  print $ "Your guess was " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      print "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      print
        "This character was in the word, filling in the other word accordingly."
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      print "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

calcCharIncorrection :: Puzzle -> Int
calcCharIncorrection (Puzzle wordToGuess _ guessed) = incorrection
 where
  correction   = length $ guessed `intersect` wordToGuess
  incorrection = length guessed - correction

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ guessed) = when cond $ do
  print "You lose !"
  print $ "The word was: " ++ wordToGuess
  exitSuccess
  where cond = calcCharIncorrection puzzle > maxWordLength

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = when (all isJust filledInSoFar) $ do
  print "You win!"
  exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  print $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> print "Your guess must be a single character."

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWords'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

