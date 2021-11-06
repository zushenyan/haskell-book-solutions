module TryExcept where

import Control.Exception

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

report :: Show e => IO (Either e a) -> IO ()
report action = do
  result <- action
  case result of
    Left e -> print e
    Right _ -> return ()

willFail :: Integer -> IO ()
willFail = report . willIFail
