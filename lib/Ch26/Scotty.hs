{-# LANGUAGE OverloadedStrings #-}

module Ch26.Scotty where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT (..))

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    lift . putStrLn $ "hello"
    -- (ActionT . lift . lift . lift) . putStrLn $ "hello"
    -- ( ActionT
    --     . (ExceptT . liftM Right)
    --     . (ReaderT . const)
    --     . \m ->
    --       StateT $
    --         ( \s -> do
    --             a <- m
    --             return (a, s)
    --         )
    -- ) . putStrLn $ "hello"

    html $ mconcat ["<h1>scotty, ", beam, " me up</h1>"]