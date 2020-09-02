module CipherTest where

import Ch11ChapterExercises
  ( vigCeaser,
    vigDeceaser,
  )
import Cipher
  ( ceaser,
    unceaser,
  )
import Test.QuickCheck

newtype UpperString = UpperString String deriving (Show)

newtype LowerString = LowerString String deriving (Show)

instance Arbitrary UpperString where
  arbitrary = UpperString <$> listOf (elements ['A' .. 'Z'])

instance Arbitrary LowerString where
  arbitrary = LowerString <$> listOf (elements ['a' .. 'z'])

runQcCeaser :: IO ()
runQcCeaser = quickCheck prop_ceaser
  where
    prop_ceaser :: Int -> LowerString -> Bool
    prop_ceaser shift (LowerString str) = str == (unceaser shift . ceaser shift $ str)

runQcVigceaser :: IO ()
runQcVigceaser = quickCheck prop_vigCeaser
  where
    prop_vigCeaser :: UpperString -> UpperString -> Bool
    prop_vigCeaser (UpperString keyword) (UpperString str) = str == (vigDeceaser keyword . vigCeaser keyword $ str)