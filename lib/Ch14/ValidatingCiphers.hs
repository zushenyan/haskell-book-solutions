module Ch14.ValidatingCiphers where

import Ch11.Ciphers (caesar, uncaesar)
import Ch11.Ciphers2 (vigCeaser, vigDeceaser)
import Test.QuickCheck

genLetter :: Gen Char
genLetter = elements ['A' .. 'Z']

genLetter' :: Gen Char
genLetter' = elements $ ' ' : ['A' .. 'Z']

genWord :: Gen String
genWord = listOf1 genLetter'

genKeyword :: Gen String
genKeyword = listOf1 genLetter

genArg :: Gen (String, String)
genArg = do
  a <- genWord
  b <- genKeyword
  return (a, b)

main :: IO ()
main = do
  quickCheck . forAll genArg $ f1
  quickCheck . forAll genArg $ f2
  where
    f1 :: (String, String) -> Bool
    f1 (w, k) = uncaesar (caesar w k) k == w
    f2 :: (String, String) -> Bool
    f2 (w, k) = vigDeceaser k (vigCeaser k w) == w