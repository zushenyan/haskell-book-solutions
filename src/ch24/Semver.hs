module Semver where

import Control.Applicative ()
import Data.ByteString (ByteString)
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Read (readMaybe)
import Text.Trifecta
  ( Parser,
    Result (Success),
    alphaNum,
    char,
    decimal,
    eof,
    many,
    oneOf,
    parseString,
    skipMany,
    some,
    try,
  )

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer major minor patch _ _) (SemVer major' minor' patch' _ _) = compare (major + minor + patch) (major' + minor' + patch')

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  preRelease <- parsePreRelease
  build <- parseBuild
  eof
  return $ SemVer major minor patch preRelease build
  where
    parsePreRelease = parseRest "-."
    parseBuild = parseRest "+."

parseRest :: String -> Parser [NumberOrString]
parseRest identifiers = many $ do
  _ <- oneOf identifiers
  v <- some alphaNum
  case (readMaybe v :: Maybe Integer) of
    Just n -> return $ NOSI n
    _ -> return $ NOSS v

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

test :: IO ()
test = hspec $ do
  let ps = parseString parseSemVer mempty
      r' = maybeSuccess . ps
  describe "parse simple version" $
    it "2.1.1" $
      r' "2.1.1" `shouldBe` Just (SemVer 2 1 1 [] [])
  describe "parse verbose version" $ do
    it "1.0.0-x.7.z.92" $
      r' "1.0.0-x.7.z.92" `shouldBe` Just (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
    it "1.0.0-gamma+002" $
      r' "1.0.0-gamma+002" `shouldBe` Just (SemVer 1 0 0 [NOSS "gamma"] [NOSI 2])
    it "1.0.0-beta+oof.sha.41af286" $
      r' "1.0.0-beta+oof.sha.41af286" `shouldBe` Just (SemVer 1 0 0 [NOSS "beta"] [NOSS "oof", NOSS "sha", NOSS "41af286"])
  describe "ordering" $
    it "2.1.1 > 2.1.0" $ do
      let v1 = SemVer 2 1 1 [] []
          v2 = SemVer 2 1 0 [] []
      v1 > v2 `shouldBe` True