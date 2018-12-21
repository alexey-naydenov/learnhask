module MyTreeSpec (spec) where

import Test.Hspec
import MyTree

testTree :: MyBinaryTree Int
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

incremented :: MyBinaryTree Int
incremented = Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)

spec :: Spec
spec = do
  describe "binary tree" $ do
    it "increment" $ (mapTree (+1) testTree) `shouldBe` incremented
    it "preorder" $ (preorder testTree) `shouldBe` [2, 1, 3]
    it "inorder" $ (inorder testTree) `shouldBe` [1, 2, 3]
    it "postorder" $ (postorder testTree) `shouldBe` [1, 3, 2]
    it "foldr" $ (foldrTree (+) 0 testTree) `shouldBe` 6
