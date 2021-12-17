module Ch11.Ciphers where

import Data.Char (chr, ord)

decrypted :: [Char]
decrypted = "MEET AT DAWN"

encrypted :: [Char]
encrypted = "MPPR AE OYWY"

keyword :: [Char]
keyword = "ALLY"

vig :: (Int -> Int -> Int) -> Char -> Char -> Char
vig f a b = chr . (+ ord 'A') $ f (ord a) (ord b) `mod` 26

vigEncrypt :: Char -> Char -> Char
vigEncrypt = vig (+)

vigDecrypt :: Char -> Char -> Char
vigDecrypt = vig (-)

keyToWord :: [Char] -> (Int, Int) -> [Char]
keyToWord key (t, d) = take t . drop (d - t) . cycle $ key

mkAccumulatedLength :: [Char] -> [(Int, Int)]
mkAccumulatedLength str = result
  where
    wordsLengthArr = map length (words str)
    accumulatedLengthArr = drop 1 . reverse . scanr (+) 0 $ wordsLengthArr
    result = zip wordsLengthArr accumulatedLengthArr

zipStringAndCipher :: [Char] -> [Char] -> [[(Char, Char)]]
zipStringAndCipher str key = result
  where
    intermediateCipher = map (keyToWord key) $ mkAccumulatedLength str
    zipped = zip (words str) intermediateCipher
    result = map (uncurry zip) zipped

caesar :: [Char] -> [Char] -> [Char]
caesar str key = unwords . map trans $ zipStringAndCipher str key
  where
    trans = map (uncurry vigEncrypt)

uncaesar :: [Char] -> [Char] -> [Char]
uncaesar str key = unwords . map trans $ zipStringAndCipher str key
  where
    trans = map (uncurry vigDecrypt)

main :: IO ()
main = do
  putStrLn $ if caesar decrypted keyword == encrypted then "caesar works" else "caesar not working"
  putStrLn $ if uncaesar encrypted keyword == decrypted then "decaesar works" else "decaesar not working"