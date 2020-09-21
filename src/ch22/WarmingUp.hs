module WarmingUp where

import Data.Char (toUpper)
import Test.Hspec (describe, hspec, it, shouldBe)

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = rev <$> cap

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupledDo :: String -> (String, String)
tupledDo = do
  x <- cap
  y <- rev
  return (x, y)

tupledM :: String -> (String, String)
tupledM =
  cap
    >>= \a ->
      rev
        >>= \b -> return (a, b)

test :: IO ()
test = hspec $
  describe "WarmingUp" $ do
    it "composed" $
      composed "Julie" `shouldBe` "EILUJ"
    it "fmapped" $
      fmapped "Chris" `shouldBe` "SIRHC"
    it "tupled" $
      tupled "Julie" `shouldBe` ("JULIE", "eiluJ")
    it "tupledDo" $
      tupledDo "Julie" `shouldBe` ("JULIE", "eiluJ")
    it "tupledM" $
      tupledM "Julie" `shouldBe` ("JULIE", "eiluJ")