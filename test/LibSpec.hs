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
