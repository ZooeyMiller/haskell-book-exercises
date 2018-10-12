data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f (Cons x y) = Cons (f x) (fmap f y)
    fmap _ Nil        = Nil


instance Applicative List where
    pure x = Cons x (Nil)
    (<*>) Nil _          = Nil
    (<*>) _ Nil          = Nil
    (<*>) (Cons f fs) xs = (fmap f xs) `append` (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys
