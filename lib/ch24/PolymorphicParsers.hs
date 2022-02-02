{-# LANGUAGE OverloadedStrings #-}

module Ch24.PolymorphicParsers where

import Control.Applicative
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString as AB
-- import Data.Attoparsec.Text (parseOnly)

import Data.ByteString (ByteString)
import Data.Ratio
import Data.String
import Text.Parsec (Parsec, parseTest)
import Text.Trifecta hiding (parseTest)

-- badFraction :: IsString s => s
-- badFraction = "1/0"

-- alsoBad :: IsString s => s
-- alsoBad = "10"

-- shouldWork :: IsString s => s
-- shouldWork = "1/2"

-- shouldAlsoWork :: IsString s => s
-- shouldAlsoWork = "2/1"

-- parseFraction :: (MonadFail m, TokenParsing m) => m Rational
-- parseFraction = do
--   numerator <- decimal
--   char '/'
--   denominator <- decimal
--   case denominator of
--     0 -> fail "denominator cannot be zero"
--     _ -> return $ numerator % denominator

-- main :: IO ()
-- main = do
--   let attoP = parseOnly parseFraction
--       p = parseString parseFraction mempty
--   print "=== attoparsec ==="
--   print $ attoP badFraction
--   print $ attoP shouldWork
--   print $ attoP shouldAlsoWork
--   print $ attoP alsoBad

--   print "=== trifecta ==="
--   print $ p badFraction
--   print $ p shouldWork
--   print $ p shouldAlsoWork
--   print $ p alsoBad

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => AB.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  print "trifP"
  trifP nobackParse "13"
  trifP tryParse "13"
  trifP tryAnnot "13"

  print "parsec"
  parsecP nobackParse "13"
  parsecP tryParse "13"
  parsecP tryAnnot "13"

  print "attoparsec"
  attoP nobackParse "13"
  attoP tryParse "13"
  attoP tryAnnot "13"