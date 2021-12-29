module Ch17.FixerUpper where

-- 1.
q1 :: Maybe [Char]
q1 = const <$> Just "Hello" <*> pure "Hello"

-- 2.
q2 :: Maybe (Integer, Integer, [Char], [Integer])
q2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
