import           Data.Monoid

data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
    foldr f z (Constant x) = f x z
    foldMap f (Constant x) = f x

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
    foldr f z (Two _ x) = f x z
    foldMap f (Two _ x) = f x

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
    foldr f z (Three _ _ x) = f x z
    foldMap f (Three _ _ x) = f x

data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
    foldr f z (Three' _ x y) = f x $ f y z
    foldMap f (Three' _ x y) = f x <> f y

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
    foldr f z' (Four' _ x y z) = f x $ f y $ f z z'
    foldMap f (Four' _ x y z) = f x <> f y <> f z

filterF :: (Applicative f, Foldable t, Monoid (f a))
                => (a -> Bool) -> t a -> f a
filterF f xs = foldMap (\x -> if f x then pure x else mempty) xs
