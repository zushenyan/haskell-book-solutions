module InteractiveCipher where

import           Text.Read                      ( readMaybe )
import           Control.Monad                  ( forever )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           Cipher                         ( ceaser
                                                , unceaser
                                                )
import           Ch11ChapterExercises           ( vigCeaser
                                                , vigDeceaser
                                                )

runCeaser :: IO ()
runCeaser = do
  putStr "Please enter your word: "
  word <- getLine
  putStr "Please enter the shift number: "
  shiftNum <- getLine
  case (readMaybe shiftNum :: Maybe Int) of
    (Just num) -> print $ "The result is: " ++ ceaser num word
    Nothing    -> print "It's not a valid number!"

runUnceaser :: IO ()
runUnceaser = do
  putStr "Please enter your word: "
  word <- getLine
  putStr "Please enter the unshift number: "
  shiftNum <- getLine
  case (readMaybe shiftNum :: Maybe Int) of
    (Just num) -> print $ "The result is: " ++ unceaser num word
    Nothing    -> print "It's not a valid number!"

runVigCeaser :: IO ()
runVigCeaser = do
  putStr "Please enter your word: "
  word <- getLine
  putStr "Please enter the keyword: "
  keyword <- getLine
  print $ "The result is: " ++ vigCeaser keyword word

runVigDeceaser :: IO ()
runVigDeceaser = do
  putStr "Please enter your word: "
  word <- getLine
  putStr "Please enter the keyword: "
  keyword <- getLine
  print $ "The result is: " ++ vigDeceaser keyword word

run :: IO ()
run = forever $ do
  print "Pick a mode"
  print "[1] - ceaser"
  print "[2] - unceaser"
  print "[3] - vig ceaser"
  print "[4] - vig deceaser"
  print "[5] - exit"
  choice <- getLine
  case choice of
    ['1'] -> runCeaser
    ['2'] -> runUnceaser
    ['3'] -> runVigCeaser
    ['4'] -> runVigDeceaser
    ['5'] -> exitSuccess
    _     -> print $ show choice ++ " is not a valid option!"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  run
