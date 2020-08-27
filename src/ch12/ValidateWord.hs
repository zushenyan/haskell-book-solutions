module ValidateWord where

newtype Word' = Word' String deriving (Show, Eq)

vowels = "aeiou"
maxConsonant = 21

countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` vowels)

mkWord :: String -> Maybe Word'
mkWord s = if fromIntegral (countVowels s) > maxConsonant
  then Nothing
  else Just $ Word' s
