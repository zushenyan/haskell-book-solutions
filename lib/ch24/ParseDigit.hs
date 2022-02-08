module Ch24.ParseDigit where

import Control.Applicative
import Test.Hspec
import Text.Read
import Text.Trifecta

p :: Parser a -> String -> Result a
p f = parseString f mempty

p' :: Parser a -> String -> Maybe a
p' f = fromR . p f

mkInteger :: MonadFail m => String -> m Integer
mkInteger i = case readMaybe i :: Maybe Integer of
  Just a -> return a
  Nothing -> fail "failed to parse integer"

fromR :: Result a -> Maybe a
fromR (Success a) = Just a
fromR (Failure _) = Nothing

-- 2.
parseDigit :: Parser Char
parseDigit = oneOf ['0' .. '9']

base10Integer :: Parser Integer
base10Integer = some parseDigit >>= mkInteger

test1 :: IO ()
test1 = hspec $ do
  describe "test1" $ do
    it "parseDigit" $ do
      p' parseDigit "123" `shouldBe` Just '1'
      p' parseDigit "abc" `shouldBe` Nothing
    it "base10Integer" $ do
      p' base10Integer "123abc" `shouldBe` Just 123
      p' base10Integer "abc" `shouldBe` Nothing

-- 3.
base10Integer' :: Parser Integer
base10Integer' = do
  c <- oneOf "+-" <|> parseDigit
  rest <- some parseDigit
  let firstC = if c == '+' then "" else [c]
      str = firstC ++ rest
  mkInteger str

test2 :: IO ()
test2 = hspec $ do
  describe "test2" $ do
    it "base10Integer'" $ do
      p' base10Integer' "123" `shouldBe` Just 123
      p' base10Integer' "123abc" `shouldBe` Just 123
      p' base10Integer' "+123" `shouldBe` Just 123
      p' base10Integer' "-123" `shouldBe` Just (-123)
      p' base10Integer' "abc" `shouldBe` Nothing