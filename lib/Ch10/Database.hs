module Ch10.Database where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr agg []
  where
    agg :: DatabaseItem -> [UTCTime] -> [UTCTime]
    agg (DbDate v) acc = v : acc
    agg _ acc = acc

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr agg []
  where
    agg :: DatabaseItem -> [Integer] -> [Integer]
    agg (DbNumber v) acc = v : acc
    agg _ acc = acc

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . map (\(DbDate v) -> v) . filter pred
  where
    pred :: DatabaseItem -> Bool
    pred (DbDate _) = True
    pred _ = False

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . map (\(DbNumber v) -> v) . filter pred
  where
    pred :: DatabaseItem -> Bool
    pred (DbNumber _) = True
    pred _ = False

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb arr = fromIntegral (sumDb arr) / fromIntegral (size arr)
  where
    pred :: DatabaseItem -> Bool
    pred (DbNumber _) = True
    pred _ = False
    size :: [DatabaseItem] -> Int
    size = length . filter pred