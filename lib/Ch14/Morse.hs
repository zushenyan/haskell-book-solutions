module Ch14.Morse (Morse, charToMorse, morseToChar, stringToMorse, letterToMorse, morseToLetter) where

import Control.Monad (forever, when)
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF)
import Test.QuickCheck

type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse =
  M.fromList
    [ ('a', ".-"),
      ('b', "-..."),
      ('c', "-.-."),
      ('d', "-.."),
      ('e', "."),
      ('f', "..-."),
      ('g', "--."),
      ('h', "...."),
      ('i', ".."),
      ('j', ".---"),
      ('k', "-.-"),
      ('l', ".-.."),
      ('m', "--"),
      ('n', "-."),
      ('o', "---"),
      ('p', ".--."),
      ('q', "--.-"),
      ('r', ".-."),
      ('s', "..."),
      ('t', "-"),
      ('u', "..-"),
      ('v', "...-"),
      ('w', ".--"),
      ('x', "-..-"),
      ('y', "-.--"),
      ('z', "--.."),
      ('1', ".----"),
      ('2', "..---"),
      ('3', "...--"),
      ('4', "....-"),
      ('5', "....."),
      ('6', "-...."),
      ('7', "--..."),
      ('8', "---.."),
      ('9', "----."),
      ('0', "-----")
    ]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = mapM charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  line <- getLine
  convertLine line
  where
    convertLine line = do
      case stringToMorse line of
        (Just str) -> putStrLn $ unwords str
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  line <- getLine
  convertLine line
  where
    convertLine line = do
      case traverse morseToChar (words line) of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to" -> convertToMorse
        _ -> argError
    _ -> argError
  where
    argError = do
      putStrLn "Please specify the first argument as being 'from' or 'to' morse, such as: morse to"
      exitFailure

-- test
allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

test :: IO ()
test = quickCheck prop_thereAndBackAgain