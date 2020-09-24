{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.Parsec (Parsec, parseTest)
import Text.Trifecta (CharParsing, Parser, Parsing ((<?>)), char, parseString, try)

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

nobackParse :: (Monad m, CharParsing m) => m Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad m, CharParsing m) => m Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad m, CharParsing m) => m Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  -- trifecta
  trifP nobackParse "13"
  trifP tryParse "13"

  -- parsec
  parsecP nobackParse "13"
  parsecP tryParse "13"

  -- attoparsec
  attoP nobackParse "13"
  attoP tryParse "13"
