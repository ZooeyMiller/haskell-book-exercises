{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Bool

stops = "pbtdkg"

vowels = "aeiou"

-- This also solves c) of warmup and review
stopVowelStop :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStop l1 l2 = uniq [(x, y, x) | x <- l1, y <- l2]

pVowelStop :: [Char] -> [Char] -> [(Char, Char, Char)]
pVowelStop l1 l2 = uniq [('p', y, x) | x <- l1, y <- l2]

pVowelStop2 :: [Char] -> [Char] -> [(Char, Char, Char)]
pVowelStop2 l1 l2 = filter go $ stopVowelStop l1 l2
    where go (x, _, _) = x == 'p'

uniq :: Ord a => [a] -> [a]
uniq l = go l []
    where
        go [] l2 = l2
        go (x:xs) l2 = go xs newList
            where newList = if not $ elem x l2 then (l2 ++ [x]) else l2

seekritFunc :: Fractional a => String -> a
seekritFunc x =
    (/) ((fromIntegral . sum) $ map length (words x)) -- total length of words in string
               ((fromIntegral . length) (words x)) -- amount of words in a string

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = foldr ((||) . f) False l

-- shout outs to @MattJOlson for this solution as I couldn't work out
-- how to compose a 2 arity function to a 1 arity function, only performing
-- the execution of the second function on the return value of
-- the first function when all arguments were applied
myAnyPf :: (a -> Bool) -> [a] -> Bool
myAnyPf = (myOr .) . map

-- And then with that knowledge, worked this one out on my own!!!
-- cool!!!
myElemFold :: Eq a => a -> [a] -> Bool
myElemFold = (flip foldr) False . (((||) .) . (==))

myElemAny  :: Eq a => a -> [a] -> Bool
myElemAny = any . (==)

myReverse :: [a] -> [a]
myReverse = foldr ((flip (++)) . (:[])) []

myMap :: (a -> b) -> [a] -> [b]
myMap = (flip foldr) [] . (((:) .))

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = foldr (\x xs -> if f x then x : xs else xs)  [] l

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = (flip foldr) [] . ((++) .)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
                -> [a]
                -> a
myMaximumBy f (l:ls) = foldl (\x y -> if (f x y) == GT then x else y) l ls

myMinimumBy :: (a -> a -> Ordering)
                -> [a]
                -> a
myMinimumBy f (l:ls) = foldl (\x y -> if (f x y) == LT then x else y) l ls
