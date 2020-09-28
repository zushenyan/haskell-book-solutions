module FixTheCode where

import Control.Monad (guard)
import Control.Monad.IO.Class ()
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)