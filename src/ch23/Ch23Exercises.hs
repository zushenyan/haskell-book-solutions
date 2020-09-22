module Ch23Exercises where

import WriteStateYourself

-- 1.
get :: Moi s s
get = Moi $ \s -> (s, s)

run1 :: (String, String)
run1 = runMoi get "foo"

-- 2.
put :: s -> Moi s ()
put s = Moi $ const ((), s)

run2 :: ((), String)
run2 = runMoi (put "foo") "bar"

-- 3.
exec :: Moi s a -> s -> s
exec (Moi sa) s = let (a1, s1) = sa s in s1

run3 :: IO ()
run3 = do
  print $ exec (put "wilma") "dpahna"
  print $ exec get "scooby papu"

-- 4.
eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

-- 5.
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)

run5 :: IO ()
run5 = do
  let f = modify (+ 1)
  print $ runMoi f 0
  print $ runMoi (f >> f) 0