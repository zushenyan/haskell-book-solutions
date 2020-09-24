module TryTry where

import Control.Applicative ((<|>))
import Data.Ratio ((%))
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Parsing (eof, try),
    decimal,
    parseString,
    some,
  )

type DecimalOrFraction = Either Rational Integer

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Integer
parseDecimal = do
  v <- decimal
  eof
  return v

parseDof :: Parser DecimalOrFraction
parseDof =
  let l = Left <$> try parseFraction
      r = Right <$> try parseDecimal
   in l <|> r

main :: IO ()
main = do
  let p f = parseString f mempty
  print $ p (some parseDof) "123"
  print $ p (some parseDof) "1/2"
  print $ p (some parseDof) "123/"
  print $ p (some parseDof) "1/0"
