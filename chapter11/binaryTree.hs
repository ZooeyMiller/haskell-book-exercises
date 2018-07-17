import Data.List

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
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)
                
testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

preorder :: BinaryTree a -> [a]
preorder tree = go tree []
    where
        go Leaf _ = []
        go (Node left x right) xs = (x : xs) ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder tree = go tree []
    where
        go Leaf _ = []
        go (Node left x right) xs = (inorder left) ++ (x : xs) ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder tree = go tree []
    where
        go Leaf _ = []
        go (Node left x right) xs = postorder left ++ postorder right ++ (x : xs)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO () 
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."


foldTree :: 
    (a -> b -> b)
    -> b
    -> BinaryTree a
    -> b
foldTree f x b = (foldr f x . preorder) b 