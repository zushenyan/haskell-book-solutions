module Ch16.DetermineFunctor where

import GHC.Arr

-- 1.
-- ans: no, the kind is *

-- 2.
-- ans: yes

-- 3.
-- ans: yes

-- 4.
newtype Mu f = Inf {outF :: f (Mu f)}

-- ans: yes, the kind is (* -> *) -> *

-- 5.
data D = D (Array Word Word) Int Int

-- ans: no, the kind is *