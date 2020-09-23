module ParsingPractice where

import Text.Parser.Combinators (eof)
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Parsing (eof),
    parseString,
    string,
  )

-- 1.
one :: Parser ()
one = char '1' >> eof

oneTwo :: Parser ()
oneTwo = char '1' >> char '2' >> eof

run1 :: IO ()
run1 = do
  let target = "123"
  print $ parseString one mempty target
  print $ parseString oneTwo mempty target

-- 2.
p123 :: String -> IO ()
p123 s =
  let p = string s
   in print $ parseString p mempty "123"

run2 :: IO ()
run2 = do
  p123 "1"
  p123 "12"
  p123 "123"

-- 3.
string' :: String -> Parser String
string' = traverse char

-- this also works
-- string' :: CharParsing m => String -> m String
-- string' = traverse char