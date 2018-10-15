
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

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f (Cons x y) = Cons (f x) (fmap f y)
    fmap _ Nil        = Nil

myListFoldR :: (a -> b -> b) -> b -> List a -> b
myListFoldR f v (Cons x l) = f x (myListFoldR f v l)
myListFoldR f v Nil = v

instance Monoid (List a) where
    mempty = Nil
    mappend Nil ys         = ys
    mappend ys Nil         = ys
    mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

myListConcat :: List (List a) -> List a   
myListConcat = myListFoldR mappend Nil

instance Applicative List where
    pure x = Cons x (Nil)
    (<*>) Nil _          = Nil
    (<*>) _ Nil          = Nil
    (<*>) (Cons f fs) xs = (fmap f xs) `mappend` (fs <*> xs)

instance Monad List where
    return = pure
    (>>=) l f = myListConcat $ fmap f l