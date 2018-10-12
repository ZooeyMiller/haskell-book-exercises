tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        (_, d) = xLast `divMod` 10

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where xLast = x `div` 100
        d = xLast `mod` 10 
