data BinaryTree a =
     Leaf
    | Node (BinaryTree a) a (BinaryTree a) 
    deriving (Eq, Ord, Show)

insert' :: Ord a 
            => a
            -> BinaryTree a
            -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
                | b == a = Node left a right
                | b<a = Node (insert' b left) a right 
                | b>a = Node left a (insert' b right)

            
mapTree :: (a -> b)
            -> BinaryTree a
            -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node Leaf a Leaf
                