module CanadaPhone where

import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Read (readMaybe)
import Text.Trifecta
  ( Parser,
    Result (Success),
    anyChar,
    choice,
    digit,
    dot,
    eof,
    many,
    oneOf,
    optional,
    parens,
    parseString,
    some,
    string,
  )

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = PhoneNumber <$> parseNumberingPlanArea <*> parseExchange <*> parseLineNumber

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = choice [inParens, country, simple]
  where
    simple = parse3Digits
    inParens = parens simple
    country = string "1-" >> simple

parseExchange :: Parser Exchange
parseExchange = optional (oneOf " -") >> parse3Digits

parseLineNumber :: Parser LineNumber
parseLineNumber = optional (oneOf " -") >> parse4Digits

-- "take 3 <$> some digit" doesn't work as "some" will iterate until it meets empty first
parse3Digits :: Parser Int
parse3Digits = do
  a <- digit
  b <- digit
  c <- digit
  toInt [a, b, c]

parse4Digits :: Parser Int
parse4Digits = do
  a <- digit
  b <- digit
  c <- digit
  d <- digit
  toInt [a, b, c, d]

toInt :: MonadFail m => String -> m Int
toInt v =
  case (readMaybe v :: Maybe Int) of
    Just a -> return a
    _ -> fail "expected integer"

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = do
  let ps = parseString parsePhone mempty
  print $ ps "123-456-7890"
  print $ ps "1234567890"
  print $ ps "(123) 456-7890"
  print $ ps "1-123-456-7890"

test :: IO ()
test = hspec $ do
  let ps = parseString parsePhone mempty
      run = maybeSuccess . ps
  describe "parsePhone" $ do
    it "123-456-7890" $
      run "123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "1234567890" $
      run "1234567890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "(123) 456-7890" $
      run "(123) 456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "1-123-456-7890" $
      run "1-123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
