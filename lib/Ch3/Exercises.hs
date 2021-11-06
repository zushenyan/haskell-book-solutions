module Ch3.Exercises where

-- binding functions
str :: String
str = "Curry is awesome!"

-- 1. and 2.

funcA :: String -> String
funcA = take (length str - 1)

funcB :: String -> Char
funcB a = a !! 4

funcC :: String -> String
funcC = drop 9

-- 3.
thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- 4.
thirdLetter' :: Int -> Char
thirdLetter' x = str !! x

-- 5.
rvrs :: String
rvrs =
  let first = take 5 str
      second = take 2 $ drop 6 str
      third = init $ drop 9 str
   in third ++ " " ++ second ++ " " ++ first