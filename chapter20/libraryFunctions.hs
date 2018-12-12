import           Data.Foldable
import           Data.Monoid
import           Test.QuickCheck

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x t = foldr (\y z -> z || y == x) False t

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' as = foldr compare Nothing as
 where
  compare x Nothing  = Just x
  compare x (Just y) = if (x < y) then (Just x) else (Just y)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' as = foldr compare Nothing as
 where
  compare x Nothing  = Just x
  compare x (Just y) = if (x > y) then (Just x) else (Just y)

null' :: Foldable t => t a -> Bool
null' = foldr (\x y -> False) True

length' :: Foldable t => t a -> Int
length' = foldr (flip $ const . (+ 1)) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldr mappend mempty

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (mappend . f) mempty xs

------

prop_sum :: [Int] -> Bool
prop_sum xs = sum xs == sum' xs

prop_product :: [Int] -> Bool
prop_product xs = product xs == product' xs

prop_elem :: Int -> [Int] -> Bool
prop_elem x xs = elem x xs == elem' x xs

prop_null :: [Int] -> Bool
prop_null xs = null xs == null' xs

prop_length :: [Int] -> Bool
prop_length xs = length xs == length' xs

prop_fold :: [Sum Int] -> Bool
prop_fold xs = fold xs == fold' xs

prop_foldMap :: [Int] -> Bool
prop_foldMap xs = foldMap f xs == foldMap' f xs where f x = Sum (x * 2)

prop_foldMapMaybe :: Maybe Int -> Bool
prop_foldMapMaybe xm = foldMap f xm == foldMap' f xm where f x = Sum (x * 2)

-- can't propcheck minimum/maximum due to my version and lib version having a different type

main :: IO ()
main = do
  quickCheck prop_sum
  quickCheck prop_product
  quickCheck prop_elem
  quickCheck prop_null
  quickCheck prop_length
  quickCheck prop_fold
  quickCheck prop_foldMap
  quickCheck prop_foldMapMaybe






