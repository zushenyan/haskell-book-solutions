module Ch14.MakeAGenRandomGeneratorForTheDatatype where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

-- 1.
gen1 :: Gen Fool
gen1 = oneof [return Fulse, return Frue]

-- 2.
gen2 :: Gen Fool
gen2 = frequency [(2, return Fulse), (1, return Frue)]