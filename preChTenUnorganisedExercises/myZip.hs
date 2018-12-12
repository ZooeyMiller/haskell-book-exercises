myZip :: [a] -> [b] -> [(a, b)]
myZip l1 l2 = go l1 l2 []
 where
  go [] _  l3 = l3
  go _  [] l3 = l3
  go l1 l2 l3 = go as bs (l3 ++ [(a, b)])
   where
    (a : as) = l1
    (b : bs) = l2

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y = go x y []
 where
  go [] _  l3 = l3
  go _  [] l3 = l3
  go l1 l2 l3 = go as bs (l3 ++ [f a b])
   where
    (a : as) = l1
    (b : bs) = l2

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith (\x y -> (x, y))
