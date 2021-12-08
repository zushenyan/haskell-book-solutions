module Ch7.ArtfulDodgy where

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = flip dodgy 2

arr :: [(String, Integer)]
arr =
  [ ("dodgy 1 1", dodgy 1 1), -- 11
    ("dodgy 2 2", dodgy 2 2), -- 22
    ("dodgy 1 2", dodgy 1 2), -- 21
    ("dodgy 2 1", dodgy 2 1), -- 12
    ("oneIsOne 1", oneIsOne 1), -- 11
    ("oneIsOne 2", oneIsOne 2), -- 21
    ("oneIsTwo 1", oneIsTwo 1), -- 21
    ("oneIsTwo 2", oneIsTwo 2), -- 22
    ("oneIsOne 3", oneIsOne 3), -- 31
    ("oneIsTwo 3", oneIsTwo 3) -- 23
  ]

run :: IO ()
run = do
  mapM_ (\(t, v) -> print $ t ++ ": " ++ show v) arr