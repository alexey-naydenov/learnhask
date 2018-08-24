import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "simple tests" $ do
    it "" $ shouldBe (sumSquares 2) 5
