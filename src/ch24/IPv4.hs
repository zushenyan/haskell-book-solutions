{-# LANGUAGE OverloadedStrings #-}

module IPv4 where

import Data.Word (Word32)
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Read (readMaybe)
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Result (Success),
    digit,
    parseString,
    some,
  )

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

ipv4ToNumeric :: Int -> Int -> Int -> Int -> Int
ipv4ToNumeric a b c d =
  2 ^ 24 * a
    + 2 ^ 16 * b
    + 2 ^ 8 * c
    + d

converter :: Int -> Word32
converter = fromIntegral

parseIPv4 :: Parser IPAddress
parseIPv4 = do
  a <- parseInt
  char '.'
  b <- parseInt
  char '.'
  c <- parseInt
  char '.'
  d <- parseInt
  let num = ipv4ToNumeric a b c d
  return $ IPAddress (converter num)

parseInt :: Parser Int
parseInt = some digit >>= toInt

toInt :: MonadFail m => String -> m Int
toInt v =
  case (readMaybe v :: Maybe Int) of
    Just a -> return a
    _ -> fail "expected integer"

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
  let ps = parseString parseIPv4 mempty
      run = maybeSuccess . ps
  describe "IPv4" $ do
    it "172.16.254.1" $
      run "172.16.254.1" `shouldBe` Just (IPAddress 2886794753)
    it "204.120.0.15" $
      run "204.120.0.15" `shouldBe` Just (IPAddress 3430416399)