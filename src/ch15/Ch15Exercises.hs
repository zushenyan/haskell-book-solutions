module Ch15Exercises where

import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary, elements, quickCheck)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

run1 :: IO ()
run1 = do
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

-- 2.
newtype Identity a = Identity a deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

run2 :: IO ()
run2 = do
  quickCheck (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  quickCheck (monoidLeftIdentity :: Identity (Maybe String) -> Bool)
  quickCheck (monoidRightIdentity :: Identity (Maybe String) -> Bool)

-- 3.
data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x x') <> (Two y y') = Two (x <> y) (x' <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

run3 :: IO ()
run3 = do
  quickCheck (semigroupAssoc :: Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool)
  quickCheck (monoidLeftIdentity :: Two String (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Two String (Sum Int) -> Bool)

-- 4.
data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x x' x'') <> (Three y y' y'') = Three (x <> y) (x' <> y') (x'' <> y'')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

run4 :: IO ()
run4 = do
  quickCheck
    ( semigroupAssoc ::
        Three String (Sum Int) (Product Int) ->
        Three String (Sum Int) (Product Int) ->
        Three String (Sum Int) (Product Int) ->
        Bool
    )
  quickCheck (monoidLeftIdentity :: Three String (Sum Int) (Product Int) -> Bool)
  quickCheck (monoidRightIdentity :: Three String (Sum Int) (Product Int) -> Bool)

-- 5.
data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four x x' x'' x''') <> (Four y y' y'' y''') = Four (x <> y) (x' <> y') (x'' <> y'') (x''' <> y''')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

run5 :: IO ()
run5 = do
  quickCheck
    ( semigroupAssoc ::
        Four String (Sum Int) (Product Int) [String] ->
        Four String (Sum Int) (Product Int) [String] ->
        Four String (Sum Int) (Product Int) [String] ->
        Bool
    )
  quickCheck (monoidLeftIdentity :: Four String (Sum Int) (Product Int) [String] -> Bool)
  quickCheck (monoidRightIdentity :: Four String (Sum Int) (Product Int) [String] -> Bool)

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return (BoolConj x)

run6 :: IO ()
run6 = do
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return (BoolDisj x)

run7 :: IO ()
run7 = do
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

-- 8.
data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _ <> (Snd y) = Snd y
  _ <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

run8 :: IO ()
run8 = quickCheck (semigroupAssoc :: Or String Int -> Or String Int -> Or String Int -> Bool)

-- 9.
newtype Combine a b = Combine {unCombine :: a -> b}

instance Show (Combine a b) where
  show _ = "Combine a b"

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\x -> f x <> g x)

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    x <- arbitrary
    return (Combine (const x))

test9 :: IO ()
test9 =
  let f = Combine $ \x -> Sum (x + 1)
      g = Combine $ \x -> Sum (x - 1)
   in do
        print $ unCombine (f <> g) 0
        print $ unCombine (f <> g) 1

-- 10.
newtype Comp a = Comp {unComp :: a -> a}

instance Show (Comp a) where
  show _ = "Comp a"

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (\x -> f x <> g x)

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp (const mempty)
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    x <- arbitrary
    return (Comp (const x))

--  11.
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success x <> _ = Success x
  _ <> Success y = Success y
  Failure x <> Failure y = Failure (x <> y)

run11 :: IO ()
run11 = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

-- monoid exercise 8.
newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem go
    where
      go s =
        let (a, s') = f s
            (a', s'') = g s'
         in (a <> a', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))

f' :: Mem Integer String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft -- ("hi",1)
  print rmright --  ("hi",1)
  print (rmzero :: (String, Int)) --  ("",0)
  print $ rmleft == runMem f' 0 -- True
  print $ rmright == runMem f' 0 -- True
