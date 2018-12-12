import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

data PhhhbbtttEither b a = Left' a | Right' b deriving (Show, Eq)

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
myListFoldR f v Nil        = v

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

j :: Monad m => m (m a) -> m a
j x = (>>=) x id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as f = go as f (return [])
 where
  go :: Monad m => [a] -> (a -> m b) -> m [b] -> m [b]
  go []       _ l = fmap reverse l
  go (x : xs) g l = go xs g $ (:) <$> (g x) <*> l

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
    arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = oneof [return Nil, Cons <$>  arbitrary <*> arbitrary]

instance EqProp (Nope a) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq
instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Eq a => EqProp (List a) where (=-=) = eq

nopeTrigger = undefined :: Nope (Int, Int, Int)
eitherTrigger = undefined :: PhhhbbtttEither (Int, Int, Int) (Int, Int, Int)
identityTrigger = undefined :: Identity (Int, Int, Int)
listTrigger = undefined :: List (Int, Int, Int)

main :: IO ()
main = do
  quickBatch (applicative identityTrigger)
  quickBatch (monad identityTrigger)
  quickBatch (applicative listTrigger)
  quickBatch (monad listTrigger)
  quickBatch (applicative eitherTrigger)
  quickBatch (monad eitherTrigger)
  quickBatch (applicative nopeTrigger)
  quickBatch (monad nopeTrigger)
