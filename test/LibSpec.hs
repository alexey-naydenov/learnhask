module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "simple tests" $ do
    it "" $ (sumSquares 2) `shouldBe` 5
