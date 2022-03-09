module Ch24.IP where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Word
import Debug.Trace
import Numeric
import Test.Hspec
import Text.Printf
import Text.Trifecta

p :: Parser a -> String -> Result a
p f = parseString f mempty

p' :: Parser a -> String -> Maybe a
p' f s = case p f s of
  Success a -> Just a
  Failure _ -> Nothing

makeBitOffsets :: Integral a => Int -> [a]
makeBitOffsets offset = (\x -> 2 ^ (offset * (x - 1))) <$> [1 ..]

ipv4BitOffsets :: Integral a => [a]
ipv4BitOffsets = reverse . take 4 . makeBitOffsets $ 8

ipv6BitOffsets :: Integral a => [a]
ipv6BitOffsets = reverse . take 4 . makeBitOffsets $ 16

numberToBlocks :: Integral a => [a] -> a -> [a]
numberToBlocks bitOffsets num = map fst . drop 1 $ list
  where
    list = scanl (\(_, r) offset -> r `divMod` offset) (0, num) bitOffsets

blocksToNumber :: Integral a => [a] -> [a] -> a
blocksToNumber bitOffsets = sum . zipWith (*) bitOffsets

toHex :: (Eq a, Num a) => [Char] -> a
toHex str = let (x : _) = readHex str in fst x

-- IPAddress
newtype IPAddress = IPAddress Word32 deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress w) = intercalate "." . map show . numberToBlocks ipv4BitOffsets $ w

makeIPAddress :: Integral a => [a] -> IPAddress
makeIPAddress = IPAddress . fromIntegral . blocksToNumber ipv4BitOffsets

-- IPAddress6
data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

ipv6NumberToHexes :: (Integral a, Show a) => a -> [String]
ipv6NumberToHexes = numberToHexes
  where
    toHex = flip showHex ""
    numberToHexes = map toHex . numberToBlocks ipv6BitOffsets

ipv6HexesToNumber :: Integral a => [String] -> a
ipv6HexesToNumber = blocksToNumber ipv6BitOffsets . map (fromIntegral . toHex)

instance Show IPAddress6 where
  show (IPAddress6 a b) = intercalate ":" $ s1 ++ s2
    where
      s1 = ipv6NumberToHexes . fromIntegral $ a
      s2 = ipv6NumberToHexes . fromIntegral $ b

makeIPAddress6 :: [String] -> IPAddress6
makeIPAddress6 xs = IPAddress6 r1 r2
  where
    (s1, s2) = splitAt 4 xs
    r1 = ipv6HexesToNumber s1
    r2 = ipv6HexesToNumber s2

-- parsers
parseIPAddress :: Parser IPAddress
parseIPAddress = do
  w <- integer
  dot
  x <- integer
  dot
  y <- integer
  dot
  z <- integer
  return . makeIPAddress $ [w, x, y, z]

parseIPAddress6Helper :: [String] -> [String]
parseIPAddress6Helper xs =
  let ipv6Groups = 8
      gapCount = ipv6Groups - length xs + 1
      list = zip xs [0 ..]
      list' = map (\(x, i) -> if x == "" && (i == 0 || i == length list) then "0" else x) list
      trans :: String -> [String]
      trans x = if null x then replicate gapCount "0" else [x]
      result = concatMap trans list'
   in result

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  hexes <- many alphaNum `sepBy` colon
  let result = parseIPAddress6Helper hexes
  return . makeIPAddress6 $ result

-- utils
ip4to6 :: IPAddress -> IPAddress6
ip4to6 (IPAddress a) = IPAddress6 0 (fromIntegral a)

ip6to4 :: IPAddress6 -> IPAddress
ip6to4 (IPAddress6 _ b) = IPAddress (fromIntegral b)

test :: IO ()
test = hspec $ do
  describe "IPv4" $ do
    it "show" $ do
      show (IPAddress 2886794753) `shouldBe` "172.16.254.1"
      show (IPAddress 3430416399) `shouldBe` "204.120.0.15"
    it "parseIPAddress" $ do
      p' parseIPAddress "172.16.254.1" `shouldBe` Just (IPAddress 2886794753)
      p' parseIPAddress "204.120.0.15" `shouldBe` Just (IPAddress 3430416399)
  describe "IPv6" $ do
    it "ipv6HexesToNumber" $ do
      ipv6HexesToNumber ["0", "0", "0", "0"] `shouldBe` 0
      ipv6HexesToNumber ["0", "ffff", "ac10", "fe01"] `shouldBe` 281473568538113
      ipv6HexesToNumber ["fe80", "0", "0", "0"] `shouldBe` 18338657682652659712
    it "makeIPAddress6" $ do
      makeIPAddress6 ["fe80", "0", "0", "0", "0", "ffff", "ac10", "fe01"] `shouldBe` IPAddress6 18338657682652659712 281473568538113
    it "show" $ do
      show (IPAddress6 18338657682652659712 281473568538113) `shouldBe` "fe80:0:0:0:0:ffff:ac10:fe01"
      show (IPAddress6 1 1) `shouldBe` "0:0:0:1:0:0:0:1"
    it "parseIPAddress6" $ do
      let pp = p' parseIPAddress6
      pp "1::0:0:0:0:0:1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0::0:0:0:0:1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0:0::0:0:0:1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0:0:0::0:0:1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0:0:0:0::0:1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0:0:0:0:0::1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0:0:0:0::1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0:0:0::1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0:0::1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1:0::1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1::1" `shouldBe` Just (IPAddress6 281474976710656 1)
      pp "1::0" `shouldBe` Just (IPAddress6 281474976710656 0)
      pp "0::1" `shouldBe` Just (IPAddress6 0 1)
      pp "::1" `shouldBe` Just (IPAddress6 0 1)
      pp "1::" `shouldBe` Just (IPAddress6 281474976710656 0)
      pp "::" `shouldBe` Just (IPAddress6 0 0)
    describe "IPv4 <-> IPv6" $ do
      it "ip4to6" $ do
        ip4to6 (IPAddress 2886794753) `shouldBe` IPAddress6 0 2886794753
      it "ip6to4" $ do
        ip6to4 (IPAddress6 0 2886794753) `shouldBe` IPAddress 2886794753
