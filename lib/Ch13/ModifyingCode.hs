module Ch13.ModifyingCode where

import Ch11.Ciphers (caesar)
import Ch11.Ciphers2 (vigCeaser)
import Control.Monad (forever)
import Data.Char (isLetter, toLower)
import System.Exit (exitSuccess)

-- 1.
runCaesar :: IO ()
runCaesar = do
  putStrLn "Input the string: "
  str <- getLine
  putStrLn "Input the keyword: "
  keyword <- getLine
  putStrLn $ "Your encrypted string: " ++ caesar str keyword

runVigCaesar :: IO ()
runVigCaesar = do
  putStrLn "Input the string: "
  str <- getLine
  putStrLn "Input the keyword: "
  keyword <- getLine
  putStrLn $ "Your encrypted string: " ++ vigCeaser keyword str

-- 2.
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if line1 == reverse line1
    then putStrLn "It's a palindrome!"
    else
      ( do
          putStrLn "Nope!"
          exitSuccess
      )

-- 3.
palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  let str = map toLower . filter isLetter $ line1
  if str == reverse str
    then putStrLn "It's a palindrome!"
    else
      ( do
          putStrLn "Nope!"
          exitSuccess
      )

-- 4.
type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please enter the name: "
  name <- getLine
  putStrLn "Please enter the age: "
  age <- getLine
  let person = mkPerson name (read age :: Integer)
  case person of
    Left e -> putStrLn $ "Oops something went wrong: " ++ show e
    Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p