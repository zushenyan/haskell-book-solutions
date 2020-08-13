module TheFearfulSymmetry where

-- 1
myWords :: String -> [String]
myWords "" = []
myWords str =
  let s  = takeWhile (/= ' ') str
      xs = case dropWhile (/= ' ') str of
        ""        -> []
        (_ : xxs) -> xxs
  in  s : myWords xs

-- 2
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines str =
  let s  = takeWhile (/= '\n') str
      xs = case dropWhile (/= '\n') str of
        ""        -> []
        (_ : xxs) -> xxs
  in  s : myLines xs

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- 3
split :: Char -> String -> [String]
split _ "" = []
split delimiter str =
  let s  = takeWhile (/= delimiter) str
      xs = case dropWhile (/= delimiter) str of
        ""        -> []
        (_ : xxs) -> xxs
  in  s : split delimiter xs

myWords' :: String -> [String]
myWords' = split ' '

myLines' :: String -> [String]
myLines' = split '\n'
