module InstanceOfFunc where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

-- 1.
newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

run1 :: IO ()
run1 = do
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck prop_functorComp
  where
    prop_functorComp :: Fun String Int -> Fun Int Char -> Identity String -> Bool
    prop_functorComp (Fun _ f) (Fun _ g) = functorCompose f g

-- 2.
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

run2 :: IO ()
run2 = do
  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck prop_functorComp
  where
    prop_functorComp :: Fun String Int -> Fun Int Char -> Pair String -> Bool
    prop_functorComp (Fun _ f) (Fun _ g) = functorCompose f g

-- 3.
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

run3 :: IO ()
run3 = do
  quickCheck (functorIdentity :: Two Int String -> Bool)
  quickCheck prop_functorComp
  where
    prop_functorComp :: Fun String Int -> Fun Int Char -> Two Int String -> Bool
    prop_functorComp (Fun _ f) (Fun _ g) = functorCompose f g

-- 4.
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

run4 :: IO ()
run4 = do
  quickCheck (functorIdentity :: Three Char Int String -> Bool)
  quickCheck prop_functorComp
  where
    prop_functorComp :: Fun String Int -> Fun Int Char -> Three Char Int String -> Bool
    prop_functorComp (Fun _ f) (Fun _ g) = functorCompose f g

-- 5.
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

run5 :: IO ()
run5 = do
  quickCheck (functorIdentity :: Three' Int String -> Bool)
  quickCheck prop_functorComp
  where
    prop_functorComp :: Fun String Int -> Fun Int Char -> Three' Int String -> Bool
    prop_functorComp (Fun _ f) (Fun _ g) = functorCompose f g

-- 6.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

run6 :: IO ()
run6 = do
  quickCheck (functorIdentity :: Four (Maybe Int) Char Int String -> Bool)
  quickCheck prop_functorComp
  where
    prop_functorComp :: Fun String Int -> Fun Int Char -> Four (Maybe Int) Char Int String -> Bool
    prop_functorComp (Fun _ f) (Fun _ g) = functorCompose f g

-- 7.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b

run7 :: IO ()
run7 = do
  quickCheck (functorIdentity :: Four' Int String -> Bool)
  quickCheck prop_functorComp
  where
    prop_functorComp :: Fun String Int -> Fun Int Char -> Four' Int String -> Bool
    prop_functorComp (Fun _ f) (Fun _ g) = functorCompose f g

-- 8.
-- not possible because the kind is * however functor requires * -> *.