module Ch17.ChapterExercises3 where

import Control.Applicative

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
-- combos a b c = (,,) <$> a <*> b <*> c
combos = liftA3 (,,)

main :: IO ()
main = print $ combos stops vowels stops