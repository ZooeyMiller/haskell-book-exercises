g :: (a -> b) -> (a, c) -> (b, c)
g fn (x, y) = (fn x, y)
