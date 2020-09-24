{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

sectionJson :: ByteString
sectionJson =
  [r|
{
  "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

data TestData = TestData
  { section :: Host,
    what :: Color
  }
  deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData
      <$> v .: "section"
      <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for TestData"

newtype Host = Host String deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected an object for Host"

type Annotation = String

data Color = Red Annotation | Blue Annotation | Yellow Annotation deriving (Eq, Show)

instance FromJSON Color where
  parseJSON (Object v) =
    (Red <$> v .: "red")
      <|> (Blue <$> v .: "blue")
      <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Exepcted an object for Color"

data NumberOrString = Numba Integer | Stringy Text deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number v) =
    case floatingOrInteger v of
      (Left _) -> fail "Must be integral number"
      (Right i) -> return $ Numba i
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = fail "NumberOrString must be number or string"

main :: IO ()
main = do
  let d :: Maybe TestData
      d = decode sectionJson
      text = "\"blah\""
      num = "123"
  print d
  print (decode text :: Maybe NumberOrString)
  print (eitherDecode text :: Either String NumberOrString)
  print (decode num :: Maybe NumberOrString)
  print (eitherDecode num :: Either String NumberOrString)
