{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

data Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi x) = Moi $ g . x
        where g (y, z) = (f y, z)

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure x = Moi $ (x,)
    (<*>) :: Moi s (a -> b)
             -> Moi s a
             -> Moi s b
    (<*>) (Moi x) (Moi y) = Moi f
        where f s = (z, s)
                where g = fst $ x s
                      z = g $ fst $ y s

instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ h
        where h s = (runMoi a) s
                where (z, q) =  f s
                      a = g z
