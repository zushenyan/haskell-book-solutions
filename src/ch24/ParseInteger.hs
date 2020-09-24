module ParseInteger where

import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Read (readMaybe)
import Text.Trifecta
  ( Parser,
    Result (Success),
    char,
    oneOf,
    optional,
    parseString,
    some,
  )

parseDigit :: Parser Char
parseDigit = oneOf ['0' .. '9']

base10Integer :: Parser Integer
base10Integer = do
  v <- some parseDigit
  mkInteger v

base10Integer' :: Parser Integer
base10Integer' = do
  result <- optional $ char '-'
  v <- some parseDigit
  case result of
    Just i -> mkInteger $ i : v
    _ -> mkInteger v

mkInteger :: MonadFail m => String -> m Integer
mkInteger v =
  case (readMaybe v :: Maybe Integer) of
    Just n -> return n
    _ -> fail "expected integer"

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
  describe "parseDigit" $ do
    let ps = parseString parseDigit mempty
        run = maybeSuccess . ps
    it "123" $
      run "123" `shouldBe` Just '1'
    it "abc" $
      run "abc" `shouldBe` Nothing
  describe "base10Integer" $ do
    let ps = parseString base10Integer mempty
        run = maybeSuccess . ps
    it "123abc" $
      run "123abc" `shouldBe` Just 123
    it "abc" $
      run "abc" `shouldBe` Nothing
  describe "base10Integer'" $ do
    let ps = parseString base10Integer' mempty
        run = maybeSuccess . ps
    it "-123abc" $
      run "-123abc" `shouldBe` Just (-123)
    it "123abc" $
      run "123abc" `shouldBe` Just 123
