module Ch23.RollYourOwn where

import Control.Monad.Trans.State
import System.Random

data Die = One | Two | Three | Four | Five | Six deriving (Eq, Show, Enum, Bounded)

intToDie :: Int -> Die
intToDie = toEnum

randDie :: (RandomGen g) => g -> (Int, g)
randDie = randomR (fromEnum (minBound :: Die), fromEnum (maxBound :: Die))

rollDie :: State StdGen Die
rollDie = intToDie <$> state randDie

-- 1.
rollsToGetTwenty :: Int -> StdGen -> Int
rollsToGetTwenty n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (0, 5) gen
         in go (sum + die) (count + 1) nextGen

-- 2.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go (0, [])
  where
    go :: (Int, [Int]) -> StdGen -> (Int, [Die])
    go (count, xs) gen
      | sum xs >= n = (count, toEnum <$> xs)
      | otherwise =
        let (die, nextGen) = randDie gen
         in go (count + 1, die : xs) nextGen
