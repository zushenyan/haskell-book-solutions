module Ch17.ChapterExercises1 where

-- 1.
-- pure :: [] -> [] a
-- (<*>) :: [] (a -> b) -> [] a -> [] b

-- 2.
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3.
-- pure :: a -> (,) a a
-- (<*>) :: (,) a (a -> b) -> (,) a a -> (,) a b

-- 3.
-- pure :: a -> (->) e a
-- (<*>) :: (->) e (a -> b) -> (->) e a -> (->) e b
