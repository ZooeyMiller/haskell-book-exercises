import Data.List

myOr :: [Bool] -> Bool 
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool 
myAny _ [] =  False
myAny f (x: xs) = f x || myAny f xs

myElemRec :: Eq a => a -> [a] -> Bool
myElemRec _ [] = False
myElemRec x (y : ys) = x == y || myElemRec x ys 

myElem :: Eq a => a -> [a] -> Bool
myElem x y = any (==x) y

myReverse :: [a] -> [a]
myReverse l = go l []
    where 
        go [] y = y
        go (x : xs) y = go xs (x : y) 

squish :: [[a]] -> [a]
squish l = go l []
        where 
            go [] y = y
            go (x : xs) y = go xs (y ++ x)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f l =  squish $ map f l

squishMapAgain :: (a -> [b]) -> [a] -> [b]
squishMapAgain f l = go (map f l) []
            where 
                go [] y = y
                go (x : xs) y = go xs (y ++ x)

squishAgain :: [[a]] -> [a]
squishAgain = squishMapAgain id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (y: ys) = go ys y
                where
                    go [] curr = curr
                    go (x : xs) curr = go xs (if (f x curr) > LT then x else curr)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (y: ys) = go ys y
                where
                    go [] curr = curr
                    go (x : xs) curr = go xs (if (f x curr) < EQ then x else curr)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare