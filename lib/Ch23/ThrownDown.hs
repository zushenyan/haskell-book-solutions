module Ch23.Exe where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Random

data Die = One | Two | Three | Four | Five | Six deriving (Eq, Show, Enum, Bounded)

intToDie :: Int -> Die
intToDie = toEnum

randDie :: (RandomGen g) => g -> (Int, g)
randDie = randomR (fromEnum (minBound :: Die), fromEnum (maxBound :: Die))

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randDie
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state randDie

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 1
      (d1, s1) = randDie s
      (d2, s2) = randDie s1
      (d3, _) = randDie s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = (,,) <$> rollDie <*> rollDie <*> rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- the result will be
-- [
--   rollDie $ mkStdGen 0,
--   rollDie $ mkStdGen 1,
--   ...,
--   rollDie $ mkStdGen n,
-- ]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (0, 5) gen
         in go (sum + die) (count + 1) nextGen

rs :: Int -> Int
rs = rollsToGetTwenty . mkStdGen

r :: IO Int
r = rs <$> randomIO

main :: IO ()
main = print =<< r
