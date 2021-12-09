module Ch9.ThyFearfulSymmetry where

-- 1.
isSpace :: Char -> Bool
isSpace a = a == ' '

isNotSpace :: Char -> Bool
isNotSpace = not . isSpace

takeWhileIsNotSpace :: String -> String
takeWhileIsNotSpace = takeWhile isNotSpace

dropWhileIsNotSpace :: String -> String
dropWhileIsNotSpace = dropWhile isNotSpace

myWords :: String -> [String]
myWords str = go str []
  where
    go :: String -> [String] -> [String]
    go [] arr = arr
    go (' ' : xs) arr = takeWhileIsNotSpace xs : go (dropWhileIsNotSpace xs) arr
    go s arr = takeWhileIsNotSpace s : go (dropWhileIsNotSpace s) arr

-- 2.
firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = words

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- 3.

myWords' :: String -> [String]
myWords' = words