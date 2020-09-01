module Palindrome where

import           Data.Char                      ( toLower
                                                , isAlpha
                                                )
import           Control.Monad
import           System.Exit                    ( exitSuccess )

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if isPalindrome line1
    then print "It's a palindrome!"
    else do
      print "Nope!"
      exitSuccess
 where
  mkSentence = filter isAlpha . map toLower
  isPalindrome s = mkSentence s == (reverse . mkSentence $ s)
