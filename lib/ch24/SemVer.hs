module Ch24.SemVer where

import Control.Applicative
import Control.Monad
import Test.Hspec
import Text.Read
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Ord, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer major minor patch _ _) (SemVer major' minor' patch' _ _) =
    (major `compare` major')
      <> (minor `compare` minor')
      <> (patch `compare` patch')

toNumberOrString :: String -> NumberOrString
toNumberOrString str = case (readMaybe str :: Maybe Integer) of
  (Just a) -> NOSI a
  _ -> NOSS str

fs :: Result a -> Maybe a
fs (Success a) = Just a
fs _ = Nothing

parseIdentifiers :: Parser [String]
parseIdentifiers = many $ many dot >> some (alphaNum <|> char '-')

parseMetadata :: Parser [String]
parseMetadata = join <$> many (char '+' >> parseIdentifiers)

parseRelease :: Parser [String]
parseRelease = join <$> many (char '-' >> parseIdentifiers)

-- a :: Parser [NumberOrString]
-- a = parseRelease >>= return . fmap toNumberOrString

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  dot
  minor <- integer
  dot
  patch <- integer
  _release <- parseRelease
  _metadata <- parseMetadata
  let release = toNumberOrString <$> _release
      metadata = toNumberOrString <$> _metadata
  return $ SemVer major minor patch release metadata

test :: IO ()
test = hspec $ do
  let ps f = fs . parseString f mempty
  describe "SemVer ordering" $ do
    it "should work" $ do
      (SemVer 2 1 1 [] [] `compare` SemVer 2 1 0 [] []) `shouldBe` GT
      (SemVer 2 1 1 [] [] `compare` SemVer 2 1 100 [] []) `shouldBe` LT
      (SemVer 2 2 1 [] [] `compare` SemVer 2 1 100 [] []) `shouldBe` GT
  describe "parseIdentifiers" $ do
    it "should work" $ do
      let p = ps parseIdentifiers
      p "x.7.z.92" `shouldBe` Just ["x", "7", "z", "92"]
      p "gamma" `shouldBe` Just ["gamma"]
      p "oof.sha.41af286" `shouldBe` Just ["oof", "sha", "41af286"]
  describe "test semantic version parsing" $ do
    it "should work" $ do
      let p = ps parseSemVer
      p "2.1.1" `shouldBe` Just (SemVer 2 1 1 [] [])
      p "1.0.0-x.7.z.92" `shouldBe` Just (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
      p "1.0.0+x.7.z.92" `shouldBe` Just (SemVer 1 0 0 [] [NOSS "x", NOSI 7, NOSS "z", NOSI 92])
      p "1.0.0-gamma+002" `shouldBe` Just (SemVer 1 0 0 [NOSS "gamma"] [NOSI 2])
      p "1.0.0-beta+oof.sha.41af286" `shouldBe` Just (SemVer 1 0 0 [NOSS "beta"] [NOSS "oof", NOSS "sha", NOSS "41af286"])
