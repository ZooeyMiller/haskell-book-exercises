recursiveSum :: (Ord a, Num a) => a -> a
recursiveSum x = go x 0
  where go x y
         | x == 0 = y
         | otherwise = go (x - 1) (x + y)
