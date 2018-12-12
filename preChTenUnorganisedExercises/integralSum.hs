integralSum :: (Integral a) => a -> a -> a
integralSum x y = go x y 0
 where
  go x y count | y == 0    = count
               | otherwise = go x (y - 1) (x + count)
