munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w 
munge f1 f2 a = v1
      where (v1, v2) = f2 (f1 a) 
