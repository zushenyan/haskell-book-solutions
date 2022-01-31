{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ch24.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

-- header
newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair $ Header <$> some letter

-- assignment
type Name = String

type Value = String

type Assignments = M.Map Name Value

skipEOL :: Parser ()
skipEOL = skipMany $ oneOf "\n"

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  val <- some $ noneOf "\n"
  skipEOL
  return (name, val)

-- comment
skipComments :: Parser ()
skipComments = do
  skipMany $ do
    char ';' <|> char '#'
    skipMany $ noneOf "\n"
    skipEOL

-- section
data Section = Section Header Assignments deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ char ' ' <|> char '\n'

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

-- config
newtype Config = Config (M.Map Header Assignments) deriving (Eq, Show)

rollup :: Section -> M.Map Header Assignments -> M.Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return $ Config mapOfSections

-- test
headerEx :: ByteString
headerEx = "[blah]"

assignmentEx :: ByteString
assignmentEx = "woot=1"

commentEx :: ByteString
commentEx =
  "; foobar\
  \ 12213"

commnetEx' :: ByteString
commnetEx' = "; blah\n; woot\n \n;hah"

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' =
  [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' =
  [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Assignment Parsing" $ do
    it "can parse a simple assignment" $ do
      let m = parseByteString parseAssignment mempty assignmentEx
          r' = maybeSuccess m
      -- print m
      r' `shouldBe` Just ("woot", "1")
  describe "Header Parsing" $ do
    it "can parse a simple header" $ do
      let m = parseByteString parseHeader mempty headerEx
          r' = maybeSuccess m
      -- print m
      r' `shouldBe` Just (Header "blah")
  describe "Comment Parsing" $ do
    it "skips comment before header" $ do
      let m = parseByteString (skipComments >> parseHeader) mempty "; woot\n[blah]"
          r' = maybeSuccess m
      -- print m
      r' `shouldBe` Just (Header "blah")
  describe "Section Parsing" $ do
    it "can parse a simple section" $ do
      let m = parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList [("Chris", "Texas")]
      -- print m
      r' `shouldBe` Just (Section (Header "states") states)
  describe "Ini Parsing" $ do
    it "can parse multiple sections" $ do
      let m = parseByteString parseIni mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = M.fromList [("alias", "claw"), ("host", "wikipedia.org")]
          whatisitValues = M.fromList [("red", "intoothandclaw")]
      -- print m
      r' `shouldBe` Just (Config (M.fromList [(Header "section", sectionValues), (Header "whatisit", whatisitValues)]))
