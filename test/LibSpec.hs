module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "isSubseqOf" $ do
    it "prefix" $ shouldBe (isSubseqOf "blah" "blahwoot") True
    it "suffix" $ shouldBe (isSubseqOf "blah" "wooblah") True
    it "interleave" $ shouldBe (isSubseqOf "blah" "woboloaht") True
  describe "capitalize words" $ do
    it "empty" $ shouldBe (capitalizeWords "") []
    it "one" $ shouldBe (capitalizeWords "test") [("test", "Test")]
    it "two" $ shouldBe (capitalizeWords "test word") [("test", "Test"),
                                                       ("word", "Word")]
    it "spaces" $ shouldBe (capitalizeWords " test   word") [("test", "Test"),
                                                             ("word", "Word")]
  describe "capitalize word" $ do
    it "empty" $ shouldBe (capitalizeWord "") ""
    it "some word" $ shouldBe (capitalizeWord "word") "Word"
    it "redundant" $ shouldBe (capitalizeWord "Word") "Word"
  describe "capitalize paragraph" $ do
    it "empty" $ shouldBe (capitalizeParagraph "") ""
    it "one word" $ shouldBe (capitalizeParagraph "word") "Word"
    it "simple" $ shouldBe (capitalizeParagraph "word. text") "Word. Text"
    it "longer" $ shouldBe (capitalizeParagraph "word another. text")
      "Word another. Text"
  describe "not the" $ do
    it "empty" $ shouldBe (notThe "") ""
    it "the" $ shouldBe (notThe "the") "a"
    it "the text" $ shouldBe (notThe "somethe") "somethe"
  describe "replace the" $ do
    it "empty" $ shouldBe (replaceThe "") ""
    it "the" $ shouldBe (replaceThe "the") "a"
    it "text" $ shouldBe (replaceThe "the some the it") "a some a it"
  describe "count vowels" $ do
    it "empty" $ shouldBe (countVowels "") 0
    it "no vowels" $ shouldBe (countVowels "xcvb") 0
    it "some vowels" $ shouldBe (countVowels "test some a") 4
  describe "extract maybe" $ do
    it "empty" $ shouldBe (catMaybe ([] :: [Maybe Int])) []
    it "nothing" $ shouldBe (catMaybe [Nothing :: Maybe Int]) []
    it "one value" $ shouldBe (catMaybe [Just 1 :: Maybe Int]) [1]
    it "two values" $ shouldBe (catMaybe ([Just 1, Just 2] :: [Maybe Int]))
      [1, 2]
    it "mixture" $ shouldBe (catMaybe ([Just 1, Nothing, Just 2] :: [Maybe Int]))
      [1, 2]
  describe "flip maybe" $ do
    it "empty" $ shouldBe (flipMaybe ([]::[Maybe Int])) (Just [])
    it "nothing" $ shouldBe (flipMaybe ([Nothing]::[Maybe Int])) Nothing
    it "just one" $ shouldBe (flipMaybe ([Just 1]::[Maybe Int])) (Just [1])
    it "a few just" $ shouldBe (flipMaybe ([Just 1, Just 2]::[Maybe Int]))
      (Just [1, 2])
    it "mixture" $ shouldBe (flipMaybe ([Just 1, Nothing]::[Maybe Int]))
      Nothing
  describe "get lefts" $ do
    it "empty" $ lefts' ([] :: [Either Char Int]) `shouldBe` []
    it "one left" $ lefts' ([Left 'a'] :: [Either Char Int]) `shouldBe` ['a']
    it "one right" $ lefts' ([Right 1] :: [Either Char Int]) `shouldBe` []
    it "mix" $ lefts' ([Left 'a', Right 1] :: [Either Char Int])
      `shouldBe` ['a']
  describe "get rights" $ do
    it "empty" $ rights' ([] :: [Either Char Int]) `shouldBe` []
    it "one left" $ rights' ([Left 'a'] :: [Either Char Int]) `shouldBe` []
    it "one right" $ rights' ([Right 1] :: [Either Char Int]) `shouldBe` [1]
    it "mix" $ rights' ([Left 'a', Right 1] :: [Either Char Int])
      `shouldBe` [1]
  describe "" $ do
