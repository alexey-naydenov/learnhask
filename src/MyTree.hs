module MyTree where

data MyBinaryTree a =
  Leaf
  | Node (MyBinaryTree a) a (MyBinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> MyBinaryTree a -> MyBinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

preorder :: MyBinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ (preorder left) ++ (preorder right)

inorder :: MyBinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = (inorder left) ++ [x] ++ (inorder right)

postorder :: MyBinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = (postorder left) ++ (postorder right) ++ [x]

foldrTree :: (a -> b -> b) -> b -> MyBinaryTree a -> b
-- foldrTree f b t = foldr f b $ inorder t
foldrTree _ b Leaf = b
foldrTree f b (Node left x right) = foldrTree f (foldrTree f (f x b) left) right

unfold :: (a -> Maybe (a, b, a)) -> a -> MyBinaryTree b
unfold f init =
  case res of
    Nothing -> Leaf
    Just (l, v, r) -> Node (unfold f l) v (unfold f r)
  where res = f init

treeBuild :: Integer -> MyBinaryTree Integer
treeBuild n = unfold f n
  where f 0 = Nothing
        f x = Just (x - 1, n - x, x - 1)
  
