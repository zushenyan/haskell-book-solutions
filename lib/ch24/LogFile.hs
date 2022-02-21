{-# LANGUAGE QuasiQuotes #-}

module Ch24.LogFile where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    optional,
  )
import Control.Monad (foldM, join, void)
import Data.Functor (void, (<&>))
import Data.List
import Data.Map (Map, fromListWith)
import Data.Maybe (fromMaybe)
import Data.Time
  ( Day,
    DiffTime,
    diffTimeToPicoseconds,
    fromGregorian,
    secondsToDiffTime,
  )
import Debug.Trace
import Test.Hspec (Expectation, anyException, context, describe, hspec, it, shouldBe, shouldThrow)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QCG
import qualified Test.QuickCheck.Random as QCR
import Text.Printf
import Text.RawString.QQ (r)
import Text.Trifecta
  ( CharParsing (anyChar, char, string),
    Parser,
    Parsing (eof, skipMany, try),
    Result (..),
    colon,
    integer,
    many,
    manyTill,
    newline,
    oneOf,
    optional,
    parseString,
    some,
    whiteSpace,
  )

-- utils
fromResult :: Result a -> Maybe a
fromResult (Success a) = Just a
fromResult (Failure _) = Nothing

p :: Parser a -> String -> Result a
p f = parseString f mempty

p' :: Parser a -> String -> Maybe a
p' f = fromResult . p f

-- examples
ex :: [Char]
ex =
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

ex2 :: String
ex2 =
  [r|
# 2022-01-01
08:00 breakfast
09:00 work
12:00 lunch
13:00 work
17:00 go home
18:00 dinner
19:00 gaming
23:00 sleep
|]

simpleEx :: String
simpleEx =
  [r|

-- rrr

# 2020-01-24
11:11 qwe
|]

sectionEx :: String
sectionEx =
  [r|# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
|]

sectionEx2 :: String
sectionEx2 =
  [r|
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
|]

sectionEx3 :: String
sectionEx3 =
  [r|
-- qwe

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
|]

sectionEx' :: Section
sectionEx' =
  Section
    (MyDay day)
    ( Logs
        [ Log (MyDiffTime diffTime1) (Description "Breakfast "),
          Log (MyDiffTime diffTime2) (Description "Bumped head, passed out"),
          Log (MyDiffTime diffTime3) (Description "Wake up, headache")
        ]
    )
  where
    day = fromGregorian 2025 2 7
    diffTime1 = 8 * 60 * 60
    diffTime2 = 9 * 60 * 60
    diffTime3 = 13 * 60 * 60 + 36 * 60

-- Comment
newtype Comment = Comment String deriving (Eq, Ord)

instance Show Comment where
  show (Comment x) = x

instance QC.Arbitrary Comment where
  arbitrary = do
    let genContent = QC.listOf . QC.elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ [' ', '&', ',']
        genSpaces = QC.listOf . return $ ' '
    s1 <- genSpaces
    comment <- genContent
    s2 <- genSpaces
    return . Comment $ "--" ++ s1 ++ comment ++ s2

-- NewlinesAndComments
newtype NewlinesAndComments = NewlinesAndComments String deriving (Eq, Ord)

instance Show NewlinesAndComments where
  show (NewlinesAndComments x) = x

instance QC.Arbitrary NewlinesAndComments where
  arbitrary = do
    temp <- QC.listOf . QC.oneof $ [return "", (QC.arbitrary :: QC.Gen Comment) <&> show]
    let result = intercalate "\n" temp ++ "\n"
    return . NewlinesAndComments $ result

-- Description
newtype Description = Description String deriving (Eq, Ord)

instance Show Description where
  show (Description s) = s

instance QC.Arbitrary Description where
  arbitrary = do
    let word = QC.listOf1 . QC.elements $ ['a' .. 'z'] ++ ['A' .. 'Z']
        words = QC.listOf1 word
        spaces = QC.sublistOf "   "
        separator = QC.oneof [spaces, return "&", return ","]
    words' <- words
    let joined =
          foldM
            ( \acc a -> do
                sep <- separator
                return $ a ++ sep ++ acc
            )
            ""
            (reverse words')
    end <- spaces
    result <- joined <&> (++ end)
    return . Description $ result

-- MyDiffTime
newtype MyDiffTime = MyDiffTime DiffTime deriving (Eq, Ord)

instance Show MyDiffTime where
  show (MyDiffTime d) = show d

showMyDiffTimeFormatted :: MyDiffTime -> String
showMyDiffTimeFormatted (MyDiffTime d) = printf "%02d:%02d" h m'
  where
    time = (`div` 10 ^ 12) . diffTimeToPicoseconds $ d
    (h, m) = time `quotRem` (60 * 60)
    (m', _) = m `quotRem` 60

instance QC.Arbitrary MyDiffTime where
  arbitrary = do
    m <- QC.chooseInteger (0, 60)
    h <- QC.chooseInteger (0, 24)
    return . MyDiffTime . secondsToDiffTime $ h * 60 * 60 + m * 60

-- Log
data Log = Log MyDiffTime Description deriving (Eq)

instance Show Log where
  show (Log diff desc) = showMyDiffTimeFormatted diff ++ " " ++ show desc

instance QC.Arbitrary Log where
  arbitrary = Log <$> QC.arbitrary <*> QC.arbitrary

-- Logs
newtype Logs = Logs [Log] deriving (Eq)

instance Show Logs where
  show (Logs xs) = foldr (\x acc -> show x ++ "\n" ++ acc) "" xs

instance QC.Arbitrary Logs where
  arbitrary = Logs <$> QC.listOf1 QC.arbitrary

-- MyDay
newtype MyDay = MyDay Day deriving (Eq)

instance Show MyDay where
  show (MyDay d) = "#" ++ " " ++ show d

instance QC.Arbitrary MyDay where
  arbitrary = do
    y <- QC.chooseInteger (1950, 2050)
    m <- QC.chooseInt (1, 12)
    d <- QC.chooseInt (1, 31)
    return . MyDay $ fromGregorian y m d

-- Section
data Section = Section MyDay Logs deriving (Eq)

instance Show Section where
  show (Section day logs) = show day ++ "\n" ++ show logs

instance QC.Arbitrary Section where
  arbitrary = Section <$> QC.arbitrary <*> QC.arbitrary

-- Sections
newtype Sections = Sections [Section] deriving (Eq)

instance Show Sections where
  show (Sections xs) = foldr (\x acc -> show x ++ acc ++ "\n") "" xs

instance QC.Arbitrary Sections where
  arbitrary = Sections <$> QC.listOf1 QC.arbitrary

-- LogFile
newtype LogFile = LogFile Sections deriving (Eq)

instance Show LogFile where
  show (LogFile xs) = show xs

instance QC.Arbitrary LogFile where
  arbitrary = LogFile <$> QC.arbitrary

-- EndOfLine
newtype EndOfLine = EndOfLine String deriving (Eq, Ord)

instance Show EndOfLine where
  show (EndOfLine x) = show x

instance QC.Arbitrary EndOfLine where
  arbitrary = EndOfLine <$> QC.sublistOf ['\n']

skipRestOfLine :: Parser ()
skipRestOfLine = void $ manyTill anyChar (void newline <|> eof)

skipComment :: Parser ()
skipComment = try (string "--") *> skipRestOfLine

skipEmpty :: Parser ()
skipEmpty = try eof <|> skipMany (try (void . some . oneOf $ "\n ") <|> try skipComment)

parseMyDiffTime :: Parser MyDiffTime
parseMyDiffTime = do
  h <- integer
  colon
  m <- integer
  return . MyDiffTime . secondsToDiffTime $ h * 60 * 60 + m * 60

parseDescription :: Parser Description
parseDescription = Description <$> manyTill anyChar (try eof <|> try (void newline) <|> try skipComment)

parseLog :: Parser Log
parseLog = Log <$> (skipEmpty *> parseMyDiffTime) <*> (parseDescription <* skipEmpty)

parseLogs :: Parser Logs
parseLogs = Logs <$> some parseLog

parseMyDay :: Parser MyDay
parseMyDay = do
  char '#'
  many $ char ' '
  y <- integer
  char '-'
  m <- integer
  char '-'
  d <- integer
  optional skipComment
  return . MyDay $ fromGregorian y (fromIntegral m) (fromIntegral d)

parseSection :: Parser Section
parseSection = Section <$> (skipEmpty *> parseMyDay) <*> (parseLogs <* skipEmpty)

parseSections :: Parser Sections
parseSections = Sections <$> some parseSection

mean :: [MyDiffTime] -> MyDiffTime
mean xs = MyDiffTime $ sum xs' / (secondsToDiffTime . fromIntegral . length $ xs')
  where
    getter (MyDiffTime d) = d
    xs' = getter <$> xs

avgTimePerDay :: Section -> Map Description MyDiffTime
avgTimePerDay (Section _ (Logs logs)) = mean <$> agg
  where
    flipPosition (Log a b) = (b, [a])
    list = flipPosition <$> logs
    agg = fromListWith (++) list

propSkipEmpty :: NewlinesAndComments -> Expectation
propSkipEmpty e = let str = "abc" in p' (skipEmpty *> string str) (show e ++ str) `shouldBe` Just str

propParseDescription :: Description -> Comment -> EndOfLine -> Expectation
propParseDescription d c e = p' parseDescription (show d ++ show c ++ show e) `shouldBe` Just d

propParseMyDiffTime :: MyDiffTime -> Expectation
propParseMyDiffTime m = p' parseMyDiffTime (showMyDiffTimeFormatted m) `shouldBe` Just m

propParseLog :: Log -> Expectation
propParseLog l = p' parseLog (show l) `shouldBe` Just l

propParseLogs :: Logs -> Expectation
propParseLogs xs = p' parseLogs (show xs) `shouldBe` Just xs

propParseMyDay :: MyDay -> Comment -> Expectation
propParseMyDay d c = p' parseMyDay (show d ++ show c) `shouldBe` Just d

propParseSection :: NewlinesAndComments -> Section -> Expectation
propParseSection cs d = p' parseSection (show cs ++ show d) `shouldBe` Just d

propParseSections :: NewlinesAndComments -> NewlinesAndComments -> Sections -> Expectation
propParseSections hcs lcs d = p' parseSections (show hcs ++ show d ++ show lcs) `shouldBe` Just d

test :: IO ()
test = hspec $ do
  describe "QuickCheck things" $ do
    prop "skipEmpty" propSkipEmpty
    prop "parseDescription" propParseDescription
    prop "parseMyDiffTime" propParseMyDiffTime
    prop "parseLog" propParseLog
    modifyMaxSize (const 40) $ it "parseLogs" $ QC.property propParseLogs
    prop "parseMyDay" propParseMyDay
    modifyMaxSize (const 40) $ it "parseSection" $ QC.property propParseSection
    modifyMaxSize (const 30) $ it "parseSections" $ QC.property propParseSections
