module FixerUpper where

-- 1.
run1 :: Maybe String
run1 = const <$> Just "Hello" <*> pure "World"

-- 2.
run2 :: Maybe (Integer, Integer, String, [Integer])
run2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
