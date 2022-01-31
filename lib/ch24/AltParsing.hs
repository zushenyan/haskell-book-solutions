{-# LANGUAGE QuasiQuotes #-}

module Ch24.AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

parseNos :: Parser NumberOrString
parseNos = skipMany (oneOf "\n") >> (Left <$> integer) <|> (Right <$> some letter)

parseNos' :: Parser NumberOrString
parseNos' = do
  skipMany (oneOf "\n")
  a <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return a

eitherOr :: String
eitherOr =
  [r|
123
abc
456
def
|]

main :: IO ()
main = do
  let p f = parseString f mempty
  print $ p (some $ token parseNos) eitherOr
  print $ p (some parseNos') eitherOr