module Ch24.CanadaPhone where

import Control.Applicative
import Data.Maybe
import Text.Read (readMaybe)
import Text.Trifecta

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

mkInteger :: String -> Int
mkInteger = fromMaybe 0 . readMaybe

p :: Parser a -> String -> Result a
p f = parseString f mempty

p1 :: Parser PhoneNumber
p1 = do
  g1 <- count 3 digit
  g2 <- count 3 digit
  g3 <- count 4 digit
  eof
  return $ PhoneNumber (mkInteger g1) (mkInteger g2) (mkInteger g3)

p2 :: Parser PhoneNumber
p2 = do
  g1 <- count 3 digit
  char '-'
  g2 <- count 3 digit
  char '-'
  g3 <- count 4 digit
  eof
  return $ PhoneNumber (mkInteger g1) (mkInteger g2) (mkInteger g3)

p3 :: Parser PhoneNumber
p3 = do
  g1 <- parens $ count 3 digit
  whiteSpace
  g2 <- count 3 digit
  char '-'
  g3 <- count 4 digit
  eof
  return $ PhoneNumber (mkInteger g1) (mkInteger g2) (mkInteger g3)

p4 :: Parser PhoneNumber
p4 = do
  digit
  char '-'
  g1 <- count 3 digit
  char '-'
  g2 <- count 3 digit
  char '-'
  g3 <- count 4 digit
  eof
  return $ PhoneNumber (mkInteger g1) (mkInteger g2) (mkInteger g3)

parsePhone :: Parser PhoneNumber
parsePhone = try p1 <|> try p2 <|> try p3 <|> try p4

run :: IO ()
run = do
  print $ p parsePhone "1234567890"
  print $ p parsePhone "123-456-7890"
  print $ p parsePhone "(123) 456-7890"
  print $ p parsePhone "1-123-456-7890"
  print $ p parsePhone "123-456-7890111"