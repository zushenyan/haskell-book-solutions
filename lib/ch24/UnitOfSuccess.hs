module Ch24.UnitOfSuccess where

import Text.Trifecta

myFunc :: Parser Integer
myFunc = do
  a <- integer
  eof
  return a

p :: String -> Result Integer
p = parseString myFunc mempty

run :: IO ()
run = do
  print $ p "123"
  print $ p "123abc"
