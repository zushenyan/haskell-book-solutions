module Ch24.ParsingPractice where

import Text.Parser.Combinators
import Text.Trifecta

-- 1.
one :: Parser Char
one = char '1'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

run1 :: IO ()
run1 = do
  print $ parseString (one >> eof) mempty "123"
  print $ parseString (oneTwo >> eof) mempty "123"

-- 2.
p123 :: String -> Result String
p123 str =
  let p = string str
   in parseString p mempty "123"

run2 :: IO ()
run2 = do
  print $ p123 "1"
  print $ p123 "12"
  print $ p123 "123"

-- 3.
myString :: String -> Parser String
myString = traverse char