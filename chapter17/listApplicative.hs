data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f (Cons x y) = Cons (f x) (fmap f y)
    fmap _ Nil        = Nil

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
    pure x = Cons x (Nil)
    (<*>) Nil _          = Nil
    (<*>) _ Nil          = Nil
    (<*>) (Cons f fs) xs = (fmap f xs) `append` (fs <*> xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

zAppend :: ZipList' a -> ZipList' a -> ZipList' a
zAppend (ZipList' xs) (ZipList' ys) = ZipList' $ xs `append` ys

instance Applicative ZipList' where
    pure x = ZipList' $ Cons x Nil
    (<*>) (ZipList' Nil) _ = ZipList' Nil
    (<*>) _ (ZipList' Nil) = ZipList' Nil
    (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs))
        = ZipList' (Cons (f x) Nil) `zAppend` ((ZipList' fs) <*> (ZipList' xs))

