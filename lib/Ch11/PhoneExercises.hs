module Ch11.PhoneExercises where

import Data.Char (isUpper, toLower)
import Data.Function (on)
import Data.List (elemIndex, find, group, intercalate, maximumBy)
import Data.Maybe (fromMaybe)

-- 1.
newtype DaPhone = DaPhone [(Char, String)] deriving (Show)

phone :: DaPhone
phone =
  DaPhone
    [ ('1', "1"),
      ('2', "abc2"),
      ('3', "def3"),
      ('4', "ghi4"),
      ('5', "jkl5"),
      ('6', "mno6"),
      ('7', "pqrs7"),
      ('8', "tuv8"),
      ('9', "wxyz9"),
      ('*', "^*"),
      ('0', "+ _0"),
      ('#', ".,")
    ]

-- 2.
convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

type Digit = Char

type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone p) c
  | isUpper c = [('*', 1), result]
  | otherwise = [result]
  where
    c' = toLower c
    pair = case find (\(_, str) -> c' `elem` str) p of
      Just a -> a
      Nothing -> error $ "Char '" ++ [c] ++ "' was not found in the phone"
    getIndex = fromMaybe 0 $ elemIndex c' (snd pair)
    result = (fst pair, getIndex + 1)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps

-- 3.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- 4.
mostPopularLetter :: String -> Char
mostPopularLetter = head . maximumBy (compare `on` length) . group

-- 5.
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = head . maximumBy (compare `on` length) . group . words . unwords