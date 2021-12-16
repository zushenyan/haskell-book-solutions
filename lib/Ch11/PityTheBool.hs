module Ch11.PityTheBool where

import Data.Int

-- 1.
-- ans: 4

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

-- 2.
-- ans:
-- 256 + 2 = 258
-- out of the Int8 range
-- out of the Int8 range

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)