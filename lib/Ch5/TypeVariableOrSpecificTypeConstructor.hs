module Ch5.TypeVariableOrSpecificTypeConstructor where

-- 1.
-- it has already been explained by the example.

-- 2.
{-
f :: zed -> Zed -> Blah
zed -> fully polymorphic
Zed -> concrete type
Blah -> concrete type
-}

-- 3.
{-
f :: Enum b => a -> b -> C
a -> fully polymorphic
b -> constrained polymorphic
C -> concrete type
-}

-- 4.
{-
f :: f -> g -> C
f -> fully polymorphic
g -> fully polymorphic
C -> concrete type
-}