{-# LANGUAGE RankNTypes #-}

module Ch16.Nat where

-- without RankNTypes
-- type Nat f g a = f a -> g a

type Nat f g = forall a. f a -> g a

-- maybeToList :: (Num a) => Nat Maybe [] a
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

main :: IO ()
main = do
  print (maybeToList Nothing :: [Int])
  print $ maybeToList (Just 1)