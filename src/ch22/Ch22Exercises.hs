module Ch22Exercises where

import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)

-- 1.
x :: [Int]
x = [1, 2, 3]

y :: [Int]
y = [4, 5, 6]

z :: [Int]
z = [7, 8, 9]

xs :: Maybe Int
xs = lookup 3 $ zip x y

ys :: Maybe Int
ys = lookup 6 $ zip y z

zs :: Maybe Int
zs = lookup 4 $ zip x y

z' :: Int -> Maybe Int
z' n = lookup n $ zip x z

x1 :: Maybe (Int, Int)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Int, Int)
x2 = (,) <$> ys <*> zs

x3 :: Int -> (Maybe Int, Maybe Int)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Int -> Bool
bolt = liftA2 (&&) (> 3) (< 8)

seqA :: Integral a => a -> [Bool]
seqA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Int
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ foldr (||) False $ seqA 5
  print $ seqA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys