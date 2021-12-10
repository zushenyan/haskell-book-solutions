module Ch9.MoreBottoms where

import Data.Bool (bool)

-- 6.
myFunc :: [Integer]
myFunc = map (\x -> bool (- x) x (x == 3)) [1 .. 10]