{-# LANGUAGE FlexibleInstances #-}

module Ch16.WriteFunctors where

import Test.QuickCheck

fi :: (Functor f, Eq (f a)) => f a -> Bool
fi x = fmap id x == x

fc :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
fc (Fun _ f) (Fun _ g) x = fmap g (fmap f x) == fmap (g . f) x

-- 1.
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = oneof [return Finance, Desk <$> arbitrary, Bloor <$> arbitrary]

test1 :: IO ()
test1 = do
  quickCheck (fi :: Quant Char Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> Quant Char Int -> Bool)

-- 2.
newtype Kat a b = Kat a deriving (Eq, Show)

instance Functor (Kat a) where
  fmap _ (Kat a) = Kat a

instance (Arbitrary a) => Arbitrary (Kat a b) where
  arbitrary = Kat <$> arbitrary

test2 :: IO ()
test2 = do
  quickCheck (fi :: Kat Char Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> Kat Char Int -> Bool)

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Kat a) where
  fmap f (Flip (Kat a)) = Flip $ Kat (f a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip Kat a b) where
  arbitrary = Flip . Kat <$> arbitrary

test3 :: IO ()
test3 = do
  quickCheck (fi :: Flip Kat Char Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> Flip Kat Char Int -> Bool)

-- 4.
newtype EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

test4 :: IO ()
test4 = do
  quickCheck (fi :: EvilGoateeConst Char Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> EvilGoateeConst Char Int -> Bool)

-- 5.
newtype LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ f <$> fa

instance (Functor f, Arbitrary (f a), Arbitrary a) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary

test5 :: IO ()
test5 = do
  quickCheck (fi :: LiftItOut Maybe Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> LiftItOut Maybe Int -> Bool)

-- 6.
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

instance (Functor f, Functor g, Arbitrary (f a), Arbitrary (g a), Arbitrary a) => Arbitrary (Parappa f g a) where
  arbitrary = DaWrappa <$> arbitrary <*> arbitrary

test6 :: IO ()
test6 = do
  quickCheck (fi :: Parappa Maybe (Either Char) Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> Parappa Maybe (Either Char) Int -> Bool)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (f <$> gb)

instance (Functor f, Functor g, Arbitrary (f a), Arbitrary (g b), Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = IgnoringSomething <$> arbitrary <*> arbitrary

test7 :: IO ()
test7 = do
  quickCheck (fi :: IgnoreOne Maybe (Either Char) Bool Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> IgnoreOne Maybe (Either Char) Bool Int -> Bool)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

instance (Functor g, Arbitrary (g o), Arbitrary (g a), Arbitrary (g t), Arbitrary o, Arbitrary a, Arbitrary t) => Arbitrary (Notorious g o a t) where
  arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary

test8 :: IO ()
test8 = do
  quickCheck (fi :: Notorious Maybe Char Bool Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> Notorious Maybe Char Bool Int -> Bool)

-- 9.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

test9 :: IO ()
test9 = do
  quickCheck (fi :: List Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> List Int -> Bool)

-- 10.
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoat (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoat fa ga ha) = MoreGoat (f <$> fa) (f <$> ga) (f <$> ha)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = oneof [return NoGoat, OneGoat <$> arbitrary, MoreGoat <$> arbitrary <*> arbitrary <*> arbitrary]

test10 :: IO ()
test10 = do
  quickCheck (fi :: GoatLord Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> GoatLord Int -> Bool)

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance (Show a) => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print s a) = "Print " ++ show s ++ " " ++ show a
  show (Read f) = "Read f"

instance (Eq a) => Eq (TalkToMe a) where
  (==) Halt Halt = True
  (==) (Print s a) (Print s' a') = s == s && a == a'
  (==) (Read f) (Read g) = f "a" == g "a"
  (==) _ _ = False

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read g) = Read (f . g)

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
  arbitrary = oneof [return Halt, Print <$> arbitrary <*> arbitrary, Read <$> arbitrary]

test11 :: IO ()
test11 = do
  quickCheck (fi :: TalkToMe Int -> Bool)
  quickCheck (fc :: Fun Int Char -> Fun Char Bool -> TalkToMe Int -> Bool)