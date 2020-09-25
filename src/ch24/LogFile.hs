{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFile where

import Control.Applicative ((<|>))
import qualified Data.Map.Lazy as M
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Text.RawString.QQ (r)
import Text.Trifecta
  ( CharParsing (char, string),
    Parser,
    Parsing (skipMany),
    Result (Failure, Success),
    count,
    digit,
    noneOf,
    oneOf,
    optional,
    parseString,
    some,
  )

sampleFile :: String
sampleFile =
  [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

data Activity = Activity UTCTime String deriving (Eq, Show)

data Schedule = Schedule UTCTime [Activity] deriving (Eq, Show)

newtype LogFile = LogFile [Schedule] deriving (Eq, Show)

skipEOL :: Parser ()
skipEOL = skipMany $ oneOf "\n"

skipComment :: Parser ()
skipComment = do
  string "--"
  skipMany $ noneOf "\n"

skipComments :: Parser ()
skipComments = skipMany $ do
  skipComment
  skipEOL

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ char ' ' <|> char '\n'

parseLog :: Parser LogFile
parseLog = LogFile <$> some parseSchedule

parseSchedule :: Parser Schedule
parseSchedule = do
  skipWhitespace
  skipComments
  d <- parseDate
  a <- some $ parseActivity d
  skipEOL
  let d' = mkUTCTime $ d ++ " 00:00"
  return $ Schedule d' a

parseDate :: Parser String
parseDate = do
  char '#'
  char ' '
  y <- some digit
  char '-'
  m <- some digit
  char '-'
  d <- some digit
  skipWhitespace
  skipComments
  skipEOL
  return $ y ++ "-" ++ m ++ "-" ++ d

parseActivity :: String -> Parser Activity
parseActivity d = do
  t <- parseActivityTime
  n <- parseActivityName
  skipEOL
  let t' = mkUTCTime $ d ++ " " ++ t
  return $ Activity t' n

parseActivityTime :: Parser String
parseActivityTime = do
  h <- count 2 digit
  char ':'
  m <- count 2 digit
  char ' '
  return $ h ++ ":" ++ m

parseActivityName :: Parser String
parseActivityName = some $ do
  n <- noneOf "\n"
  optional skipComment
  return n

mkUTCTime :: String -> UTCTime
mkUTCTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M"

getResult :: MonadFail m => Result LogFile -> m LogFile
getResult (Success a) = return a
getResult (Failure _) = fail "wee"

main :: IO ()
main = do
  result <- getResult $ parseString parseLog mempty sampleFile
  print result