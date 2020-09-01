module Ch13Exercises where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0
  = Right $ Person name age
  | name == ""
  = Left NameEmpty
  | not (age > 0)
  = Left AgeTooLow
  | otherwise
  = Left
    $  PersonInvalidUnknown
    $  "Name was: "
    ++ show name
    ++ " Age was: "
    ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Please enter a person's name: "
  name <- getLine
  putStr "Please enter a person's age: "
  age <- getLine
  case mkPerson name (read age) of
    Right (Person n a) -> do
      print "Yay! Successfully got a person: "
      print $ "Name: " ++ n
      print $ "Age: " ++ show a
    Left NameEmpty            -> print "The name cannot be empty."
    Left AgeTooLow            -> print "The age is too low."
    Left invalidReasonUnknown -> print invalidReasonUnknown
