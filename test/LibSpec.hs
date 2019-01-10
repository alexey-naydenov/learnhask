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
