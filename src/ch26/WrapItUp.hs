module WrapItUp where

import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Trans.Reader (ReaderT (ReaderT))

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = undefined

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT . ExceptT . ReaderT $ const (return (Right (Just 1)))