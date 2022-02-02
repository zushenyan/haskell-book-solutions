{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ch24.Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ

sectionJson :: ByteString
sectionJson =
  [r|
{
  "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

newtype Host = Host String deriving (Eq, Show)

data Color = Red String | Blue String | Yellow String deriving (Eq, Show)

data TestData = TestData
  { section :: Host,
    what :: Color
  }
  deriving (Show, Eq)

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "expected an object for host"

instance FromJSON Color where
  parseJSON (Object v) = (Red <$> v .: "red") <|> (Blue <$> v .: "blue") <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "expected an object for color"

instance FromJSON TestData where
  parseJSON (Object v) = TestData <$> v .: "section" <*> v .: "whatisit"
  parseJSON _ = fail "expected an object for TestData"

main :: IO ()
main = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d

-- ===

data NumberOrString = Numba Integer | Stringy Text deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _) -> fail "Must be integral number"
      (Right n) -> return $ Numba n
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = fail "NumberOrString must be number or string"

dec :: ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: ByteString -> Either String NumberOrString
eitherDec = eitherDecode

main2 :: IO ()
main2 = do
  print $ dec "123"
  print $ eitherDec "123"
  print $ dec "\"blah\""
  print $ eitherDec "\"blah\""