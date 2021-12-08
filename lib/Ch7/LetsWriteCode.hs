module Ch7.LetsWriteCode where

-- 1.
-- 1a.
tensDigit :: Integral a => a -> a
tensDigit x = fst $ divMod x 10

-- 1b.
-- ans: yes

-- 1c.
hunsD :: Integral a => a -> a
hunsD x = d
  where
    d = fst $ divMod x 100

-- 2.
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y bool = case bool of
  True -> y
  False -> x

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y bool
  | bool = y
  | otherwise = x

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4.
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main :: IO ()
main = do print (roundTrip 4)

-- 5.
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

main' :: IO ()
main' = do print (roundTrip' 4)

-- 6.
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

main'' :: IO ()
main'' = do print (roundTrip'' 4 :: Integer)