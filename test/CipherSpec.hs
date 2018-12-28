module CipherSpec (spec) where

import Test.Hspec
import Cipher

sample :: String
sample = "MEET AT DAWN"

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
  describe "Subtract letters" $ do
    it "A - A" $ (subtractLetters 'A' 'A') `shouldBe` 'A'
    it "O - N" $ (subtractLetters 'O' 'N') `shouldBe` 'B'
    it "A - B" $ (subtractLetters 'A' 'B') `shouldBe` 'Z'
  describe "Vigenere cipher" $ do
    it "encode empty" $ (vigenereEncode shiftWord "") `shouldBe` ""
    it "encode" $ (vigenereEncode shiftWord sample) `shouldBe` encrypted
    it "decode empty" $ (vigenereDecode shiftWord "") `shouldBe` ""
    it "decode" $ (vigenereDecode shiftWord encrypted) `shouldBe` sample
