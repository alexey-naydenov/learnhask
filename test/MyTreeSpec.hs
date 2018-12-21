module MyTreeSpec (spec) where

import Test.Hspec
import MyTree

spec :: Spec
spec = do
  describe "simple tests" $ do
    it "" $ (2 + 2) `shouldBe` 4
