module UnitOfSuccess where

import Text.Trifecta

myFunc :: Parser String
myFunc = do
  a <- decimal
  eof
  return $ show a

main :: IO ()
main = do
  let myParse = parseString myFunc mempty
  print $ myParse "123"
  print $ myParse "123abc"