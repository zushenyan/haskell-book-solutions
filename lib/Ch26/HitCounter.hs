{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ch26.HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config
  { -- that's one, one click!
    -- two... two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer),
    prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp ::
  Text ->
  M.Map Text Integer ->
  (M.Map Text Integer, Integer)
bumpBoomp k m = (m', v)
  where
    v = 1 + M.findWithDefault 0 k m
    m' = M.insert k v m

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  prefix <- lift $ asks prefix
  let key' = mappend prefix unprefixed
  configRef <- lift $ asks counts
  lift . lift $ modifyIORef configRef (fst . bumpBoomp key')

  newInteger <- lift . lift $ fromMaybe 0 . M.lookup key' <$> readIORef configRef
  html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR :: ReaderT Config IO a -> IO a
      runR m = runReaderT m config
  scottyT 3000 runR app