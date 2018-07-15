-- {-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

newtype Tup =
    Tup (String, Int)
    deriving (Eq, Show, TooMany)

instance TooMany (String, Int) where
    tooMany = tooMany . snd

instance TooMany (Int, Int) where
    tooMany = tooMany . uncurry (+)

-- newtype Tup2 =
--     Tup2 (Int, Int)
--     deriving (Eq, Show, TooMany)

-- instance (Num a, TooMany a) => (a, a) where
-- tooMany = tooMany . (/2) . uncurry (+)

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany = tooMany . uncurry (+)
