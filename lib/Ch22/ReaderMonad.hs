{-# LANGUAGE InstanceSigs #-}

module Ch22.ReaderMonad where

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r $ ra r

-- 1.
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) aRb = Reader $ \r -> runReader (aRb (ra r)) r

-- 2.
newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

getDogRM :: Reader Person Dog
getDogRM = Reader $ Dog <$> dogName <*> address

run2 :: IO ()
run2 = print $ runReader getDogRM pers