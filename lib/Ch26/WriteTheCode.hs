module Ch26.WriteTheCode where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity
import Test.Hspec

-- 1,2.
rDec :: Num a => Reader a a
rDec = ReaderT $ return . subtract 1

test1 :: IO ()
test1 = hspec $ do
  it "should work" $ do
    runReader rDec 1 `shouldBe` 0
    fmap (runReader rDec) [1 .. 10] `shouldBe` [0 .. 9]

-- 3,4.
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ return . show

test2 :: IO ()
test2 = hspec $ do
  it "should work" $ do
    runReader rShow 1 `shouldBe` "1"
    fmap (runReader rShow) [1 .. 10] `shouldBe` show <$> [1 .. 10]

-- 5.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  print . mconcat $ ["Hi: ", show r]
  return $ r + 1

test5 :: IO ()
test5 = do
  r <- runReaderT rPrintAndInc 1
  print r

  r2 <- traverse (runReaderT rPrintAndInc) [1 .. 10]
  print r2

-- 6.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  print s
  return (show s, s + 1)

test6 :: IO ()
test6 = do
  r <- runStateT sPrintIncAccum 10
  print r

  r2 <- mapM (runStateT sPrintIncAccum) [1 .. 5]
  print r2