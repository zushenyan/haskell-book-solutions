module Ch23.ChapterExercises where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State g) = State $ \s ->
    let (a, _) = g s
     in (f a, s)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) (State fab) (State g) = State $ \s ->
    let (a, _) = g s
        (ab, _) = fab s
     in (ab a, s)

instance Monad (State s) where
  return = pure
  (>>=) (State f) g = State $ \s ->
    let (a, _) = f s
        h = runState $ g a
     in h s

-- 1.
get :: State s s
get = State $ \s -> (s, s)

run1 :: IO ()
run1 = print $ runState get "curryIsAmaze"

-- 2.
put :: s -> State s ()
put s = State $ const ((), s)

run2 :: IO ()
run2 = print $ runState (put "blah") "woot"

-- 3.
exec :: State s a -> s -> s
exec (State sa) = snd . sa

run3 :: IO ()
run3 = do
  print $ exec (put "wilma") "daphne"
  print $ exec get "scooby papu"

-- 4.
eval :: State s a -> s -> a
eval (State sa) = fst . sa

run4 :: IO ()
run4 = do
  print $ eval (put "wilma") "daphne"
  print $ eval get "scooby papu"

-- 5.
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

run5 :: IO ()
run5 = do
  let f = modify (+ 1)
  print $ runState f 0
  print $ runState (f >> f) 0