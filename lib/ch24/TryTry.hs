module Ch24.TryTry where

import Control.Applicative
import Text.Trifecta

type DOI = Either Double Integer

roi :: Parser (Either Double Integer)
roi = (Left <$> try double) <|> (Right <$> integer)

-- roi :: Parser (Either Integer Double)
-- roi = integerOrDouble

main :: IO ()
main = do
  let p f = parseString f mempty
  print $ p roi "1.23"
  print $ p roi "123"
  print $ p roi "1.23.4"