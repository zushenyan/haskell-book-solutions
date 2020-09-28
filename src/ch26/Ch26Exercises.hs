module Ch26Exercises where

import Control.Monad.Trans.Reader
  ( Reader,
    ReaderT (..),
    reader,
    runReader,
  )
import Control.Monad.Trans.State (StateT (StateT))
import Data.Functor.Identity (Identity (Identity))
import Test.Hspec (describe, hspec, it, shouldBe)

-- 1.
-- 2.
rDec :: Num a => Reader a a
rDec = reader $ subtract 1

test1 :: IO ()
test1 =
  hspec $
    describe "rDec" $
      it "should work" $
        fmap (runReader rDec) [1 .. 10] `shouldBe` [0 .. 9]

-- 3.
-- 4.
rShow :: Show a => ReaderT a Identity String
rShow = reader show

test2 :: IO ()
test2 =
  hspec $
    describe "rShow" $
      it "should work" $
        fmap (runReaderT rShow) [1 .. 10] `shouldBe` fmap (Identity . show) [1 .. 10]

-- 5.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
  putStrLn $ "Hi " ++ show a
  return $ a + 1

-- 6.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn $ "Hi: " ++ show s
  return (show s, s + 1)
