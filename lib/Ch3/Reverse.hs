module Ch3.Reverse where

rvrs :: String -> String
rvrs str =
  let first = take 5 str
      second = take 2 $ drop 6 str
      third = init $ drop 9 str
   in third ++ " " ++ second ++ " " ++ first

main :: IO ()
main = print $ rvrs "Curry is awesome!"