{-# LANGUAGE ScopedTypeVariables #-}

module Ch26.HypotheticalExercise where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

a :: ReaderT String Maybe String
a = ReaderT $ \r -> do
  case length r of
    0 -> Nothing
    _ -> return r

b :: MaybeT (Reader String) String
b = MaybeT $ do
  a <- ask
  case length a of
    0 -> return Nothing
    _ -> return . return $ a

run :: IO ()
run = do
  print $ runReaderT a "foo"
  print $ runReaderT a ""

  print $ runReaderT (runMaybeT b) "foo"
  print $ runReaderT (runMaybeT b) ""
