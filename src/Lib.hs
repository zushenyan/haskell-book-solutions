{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text.Lazy as TL

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Class
-- import Data.Monoid
-- import Web.Scotty

-- lift :: (MonadTrans t, Monad m) => m a -> t m a
-- lift :: (MonadTrans t) => IO a -> t IO a
-- lift :: IO a -> ActionM a
-- lift :: IO () -> ActionM a

-- main :: IO ()
-- main = scotty 3000 $
--   get "/" $ do
--     liftIO $ putStrLn "test"
--     html "hello world!"