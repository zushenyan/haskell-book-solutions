module Database where

import           Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 1234
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

{- 1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them: -}
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map mPre . filter fPre
 where
  fPre (DbDate _) = True
  fPre _          = False
  mPre (DbDate x) = x

{- 2. Write a function that filters for DbNumber values and returns a list of the Integer values inside them: -}
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map mPre . filter fPre
 where
  fPre (DbNumber _) = True
  fPre _            = False
  mPre (DbNumber x) = x

{- 3. Write a function that gets the most recent date: -}
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

{- 4. Write a function that sums all of the DbNumber values: -}
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

{-
5. Write a function that gets the average of the DbNumber values:
-- You'll probably need to use fromIntegral to get from Integer to Double.
-}
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (numCount db)
  where numCount = length . filterDbNumber
