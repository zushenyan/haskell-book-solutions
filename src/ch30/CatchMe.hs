module CatchMe where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print $ typeOf e
  print $ "We errored! " ++ show e

main :: IO ()
main = do
  writeFile "zzz" "hi"
    `catch` handler
  print "lalal"