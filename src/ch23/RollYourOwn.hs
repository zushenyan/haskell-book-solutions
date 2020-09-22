module RollYourOwn where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die = One | Two | Three | Four | Five | Six deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> One
  2 -> Two
  3 -> Three
  4 -> Four
  5 -> Five
  6 -> Six
  _ -> error $ "intToDie got non 1-6 Integer: " ++ show n

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let range = (1, 6)
      s = mkStdGen 0
      (d1, s1) = randomR range s
      (d2, s2) = randomR range s1
      (d3, _) = randomR range s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 []
  where
    go :: Int -> [Int] -> StdGen -> (Int, [Die])
    go count dice gen
      | sum dice >= n = (count, intToDie <$> dice)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (count + 1) (die : dice) nextGen