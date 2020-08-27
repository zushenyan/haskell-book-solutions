module Phone where

import           Data.Char
import           Data.List

data DaPhone = DaPhone [(Char, String)] deriving (Show, Eq)

phone = DaPhone
  [ ('1', "1")
  , ('2', "abc2")
  , ('3', "def3")
  , ('4', "ghi4")
  , ('5', "jkl5")
  , ('6', "mno6")
  , ('7', "pqrs7")
  , ('8', "tuv8")
  , ('9', "wxyz9")
  , ('*', "^*")
  , ('0', "+ _0")
  , ('#', ".,#")
  ]

phone' = case phone of
  DaPhone v -> v

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol OK. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "OK. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone phone) c = if isUpper c
  then case findKeypad of
    Just (pad, chars) -> case elemIndex lowerC chars of
      Just index -> [('*', 1), (pad, index + 1)]
  else case findKeypad of
    Just (pad, chars) -> case elemIndex lowerC chars of
      Just index -> [(pad, index + 1)]
 where
  lowerC     = toLower c
  findKeypad = find (\(pad, chars) -> lowerC `elem` chars) phone

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) acc -> acc + p) 0

myMax :: (a, Int) -> (a, Int) -> (a, Int)
myMax xt@(x, count) xt'@(x', count') = case compare count count' of
  GT -> xt
  _  -> xt'

tupleMap :: [[a]] -> [(a, Int)]
tupleMap = map (\xt@(x : _) -> (x, length xt))

mostPopularLetter :: String -> Char
mostPopularLetter =
  fst . foldr1 myMax . tupleMap . group . sort . map toLower . filter isLetter

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . unlines

coolestWord :: [String] -> String
coolestWord =
  fst
    . foldr1 myMax
    . tupleMap
    . group
    . sort
    . filterEmpty
    . map filterNonLetter
    . words
    . map toLower
    . unlines
 where
  filterEmpty     = filter (/= [])
  filterNonLetter = filter isLetter
