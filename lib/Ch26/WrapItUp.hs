module Ch26.OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT eitherWrap
  where
    a :: () -> Either String (Maybe Int)
    a = const (Right (Just 1))
    readerWrap :: ReaderT () IO (Either String (Maybe Int))
    readerWrap = ReaderT . const . return $ a ()
    eitherWrap :: ExceptT String (ReaderT () IO) (Maybe Int)
    eitherWrap = ExceptT readerWrap

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

run :: IO ()
run = do
  r <- readerUnwrap ()
  print r