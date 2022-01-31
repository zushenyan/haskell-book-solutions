{-# LANGUAGE OverloadedStrings #-}

module Ch24.Exe where

import Control.Applicative
import Data.Ratio
import Text.Trifecta

badFraction :: [Char]
badFraction = "1/0"

alsoBad :: [Char]
alsoBad = "10"

shouldWork :: [Char]
shouldWork = "1/2"

shouldAlsoWork :: [Char]
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "denomintor cannot be zero"
    _ -> return $ numerator % denominator

main :: IO ()
main = do
  let pf = parseString parseFraction mempty
  print $ pf badFraction
  print $ pf alsoBad
  print $ pf shouldWork
  print $ pf shouldAlsoWork

--
type NumberOrString = Either Integer String

a :: [Char]
a = "blah"

b :: [Char]
b = "123"

c :: [Char]
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

run2 :: IO ()
run2 = do
  let p f = parseString f mempty
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c