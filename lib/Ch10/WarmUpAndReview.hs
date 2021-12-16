module Ch10.WarmUpAndReview where

-- 1.

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

nouns :: [String]
nouns = ["appla", "banana", "cat"]

verbs :: [String]
verbs = ["eat", "take", "see"]

-- 1a.
a = [(x, y, x) | x <- stops, y <- vowels]

-- 1b.
b = filter (\(x, _, _) -> x == 'p') a

-- 1c.
c = [(x, y, x) | x <- nouns, y <- verbs]

-- 2.
-- the function gets the average characters for each word in a senetence.
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- 3.
seekritFunc' :: Fractional a => String -> a
seekritFunc' x = fromIntegral (sum . map length $ words x) / fromIntegral (length . words $ x)