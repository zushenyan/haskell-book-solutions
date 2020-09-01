module StringProcessing where

-- 1.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a     = Just a

replaceThe :: String -> String
replaceThe = unwords . map pred . words
 where
  pred x = case notThe x of
    Nothing  -> "a"
    (Just a) -> a

-- 2.
vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . map notThe . words
 where
  go :: [Maybe String] -> Integer
  go []                       = 0
  go [Nothing               ] = 0
  go (Just _            : xs) = go xs
  go (Nothing : Nothing : xs) = go xs
  go (Nothing : Just [] : xs) = go xs
  go (Nothing : xt@(Just (x : _) : _)) | x `elem` vowels = 1 + go xt
                                       | otherwise       = go xt

-- 3.
countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` vowels)
