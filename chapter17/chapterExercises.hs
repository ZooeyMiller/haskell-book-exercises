import           Control.Applicative (liftA3)
import           Data.Monoid

-- Given a type specialise the types of the functions
-- from the Applicative typeclass

-- 1 - []
lPure :: a -> [a]
lPure = pure

lApply :: [(a -> b)] -> [a] -> [b]
lApply = (<*>)

-- 2 - IO
ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

-- 3 - (,) a
tupPure :: Monoid a => b -> (a, b)
tupPure = pure

tupApply :: Monoid a => (a, (c -> d)) -> (a, c) -> (a, d)
tupApply = (<*>)

-- 4 - (->) e
arPure :: Monoid a => b -> (a -> b)
arPure = pure

arApply :: Monoid a => (a -> (c -> d)) -> (a -> c) -> (a -> d)
arApply = (<*>)

-- Write instances of applicative for the following types

data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f f') (Pair x x') = Pair (f x) (f' x')

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x $ f y

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (<*>) (Two x f) (Two y z) = Two (x <> y) (f z)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (<*>) (Three x y f) (Three x' y' z) =
        Three (x <> x') (y <> y') (f z)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (<*>) (Three' x f f') (Three' y z z') =
        Three' (x <> y) (f z) (f' z')

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>)
        (Four a b c f)
        (Four x y z z') = Four (a <> x) (b <> y) (c <> z) (f z')

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c $ f d

instance Monoid a => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    (<*>)
        (Four' a b c f)
        (Four' x y z z') = Four' (a <> x) (b <> y) (c <> z) (f z')

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
