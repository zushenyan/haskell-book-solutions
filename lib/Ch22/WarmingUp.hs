module Ch22.WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> id <*> composed

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x <- id
  y <- composed
  return (x, y)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' x = (x, x) >>= (return . composed)

main :: IO ()
main = do
  print $ tupled "Julie"
  print $ tupled' "Julie"
  print $ tupled'' "Julie"