myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
    case f x of
        Just (y, z) ->
            y : myUnfoldr f z
        Nothing ->
            []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y, f y)) x


data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f x =
    case f x of
        Just (x', y, z) ->
            Node (unfoldTree f x') y (unfoldTree f z)
        Nothing ->
            Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree (go n) 0
    where go n x = if x == n then Nothing else Just (x + 1, x, x + 1)
