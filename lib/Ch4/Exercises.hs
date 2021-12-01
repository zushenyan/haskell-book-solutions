module Ch4.Exercises where

data Mood = Blah | Woot deriving (Show)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- Chapter Exercises

-- 3.
-- 6/3
-- 6 / length [1, 2, 3]
-- The second one will throw error as the (/) only takes Fractional

-- 4.
-- To fix this we can change it to like following
result :: Int
result = div 6 $ length [1 .. 3]

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.
myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))