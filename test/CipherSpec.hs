module CipherSpec (spec) where

import Test.Hspec
import Cipher

testText :: String
testText = "MEET AT DAWN"

shiftWord :: String
shiftWord = "ALLY"

encrypted :: String
encrypted = "MPPR AE OYWY"

spec :: Spec
spec = do
  describe "Add letters" $ do
    it "A + A" $ (addLetters 'A' 'A') `shouldBe` 'A'
    it "A + N" $ (addLetters 'A' 'N') `shouldBe` 'N'
    it "B + N" $ (addLetters 'B' 'N') `shouldBe` 'O'
    it "Z + B" $ (addLetters 'Z' 'B') `shouldBe` 'A'
  describe "Vigenere cipher" $ do
    it "encode empty" $ (vigenereEncode shiftWord "") `shouldBe` ""
    it "encode sample" $ (vigenereEncode shiftWord testText) `shouldBe` encrypted
