module Ch24.Exe2 where

import Text.Trifecta

p' :: Parser [Integer]
p' = some $ do
  i <- token $ some digit
  return $ read i

main :: IO ()
main = do
  print $ parseString p' mempty "1\n2\n3"
  print $ parseString (token (char 'a') >> char 'b') mempty "a b"