
data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

data PhhhbbtttEither b a = Left' a | Right' b

instance Functor (PhhhbbtttEither a) where
    fmap _ (Right' x) = Right' x
    fmap f (Left' x)  = Left' $ f x

instance Applicative (PhhhbbtttEither a) where
    pure = Left'
    (<*>) (Right' x) _        = Right' x
    (<*>) _ (Right' x)        = Right' x
    (<*>) (Left' f) (Left' x) = Left' $ f x


instance Monad (PhhhbbtttEither a) where
    return = pure
    (>>=) (Right' x) _ = Right' x
    (>>=) (Left' x) f  = f x

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) x = fmap f x

instance Monad Identity where
    return = pure
    (>>=) (Identity x) f = f x
