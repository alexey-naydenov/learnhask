module MyTree where

data MyBinaryTree a =
  Leaf
  | Node (MyBinaryTree a) a (MyBinaryTree a)

