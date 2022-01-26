module Ch23.FizzBuzzDifferently where

import Control.Monad.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  put $ fizzBuzz n : xs

-- addResult n = state $ \xs -> ((), fizzBuzz n : xs)

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

main :: IO ()
main = mapM_ putStrLn $ reverse $ fizzbuzzList [1 .. 10]

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  put . DL.snoc xs $ fizzBuzz n

fizzbuzzList' :: [Integer] -> DL.DList String
fizzbuzzList' list = execState (mapM_ addResult' list) DL.empty

main' :: IO ()
main' = mapM_ putStrLn $ fizzbuzzList' [1 .. 10]

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fizzbuzzList [from .. to]