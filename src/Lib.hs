module Lib where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = ["http://httpbin.org/ip", "http://httpbin.org/bytes/5"]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

-- f :: Int -> Maybe Int
-- f 0 = Nothing
-- f x = Just x

-- g :: Int -> Maybe Int
-- g i
--   | even i = Just $ i + 1
--   | otherwise = Nothing

-- h :: Int -> Maybe String
-- h x = Just $ "10191" ++ show x

-- doSomething :: Int -> Maybe (Int, Int, String)
-- doSomething n = do
--   a <- f n
--   b <- g a
--   c <- h b
--   pure (a, b, c)

-- doSomething :: Int -> Maybe (Int, Int, String)
-- doSomething n =
--   f n
--     >>= \a ->
--       g a
--         >>= \b ->
--           h b
--             >>= \c -> return (a, b, c)

-- data GuessWhat = ChickenButt deriving (Eq, Show)

-- data Id a = MkId a deriving (Eq, Show)

-- data Product a b = Product a b deriving (Eq, Show)

-- data Sum a b = First a | Second b deriving (Eq, Show)

-- data RecordProduct a b = RecordProduct
--   { pfirst :: a,
--     psecond :: b
--   }
--   deriving (Eq, Show)

-- newtype NumCow = NumCow Int deriving (Eq, Show)

-- newtype NumPig = NumPig Int deriving (Eq, Show)

-- newtype NumSheep = NumSheep Int deriving (Eq, Show)

-- data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

-- type Farmhouse' = Product NumCow NumPig

-- data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

-- type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

-- type Name = String

-- type Age = Int

-- type LovesMud = Bool

-- type PoundsOfWool = Int

-- data CowInfo = CowInfo Name Age deriving (Eq, Show)

-- data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

-- data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

-- data Animal
--   = Cow CowInfo
--   | Pig PigInfo
--   | Sheep SheepInfo
--   deriving (Eq, Show)

-- type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)