{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

a :: String
a = "blah"

b :: String
b = "123"

c :: String
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

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
  print $ parseString (some parseNos) mempty eitherOr

-- print $ p parseNos eitherOr

-- print $ p (some letter) a
-- print $ p integer b

-- print $ p parseNos a
-- print $ p parseNos b

-- print $ p (many parseNos) c
-- print $ p (some parseNos) c