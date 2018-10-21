{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

identityTrigger = undefined :: Identity (Int, Int, [Int])

---

newtype Constant a b =
    Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse f (Constant x) = liftA Constant (pure x)

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

constantTrigger = undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])

---

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep x) = f x

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where (=-=) = eq

optionalTrigger = undefined :: Optional (Int, Int, [Int])

---

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f (Cons x y) = Cons (f x) (fmap f y)
    fmap _ Nil        = Nil

instance Foldable List where
    foldr f z (Cons x y) = f x (foldr f z y)
    foldr _ z Nil        = z

instance Traversable List where
    sequenceA = foldr (liftA2 Cons) (pure Nil)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = oneof [return Nil, liftA2 Cons arbitrary arbitrary]

instance Eq a => EqProp (List a) where (=-=) = eq

listTrigger = undefined :: List (Int, Int, [Int])

---

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y $ f z

instance Foldable (Three a b) where
    foldr f x (Three _ _ y) = f y x

instance Traversable (Three a b) where
    -- (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
                        Arbitrary (Three a b c) where
                            arbitrary = do
                                a <- arbitrary
                                b <- arbitrary
                                c <- arbitrary
                                return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

threeTrigger = undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])

---

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x $ f y

instance Foldable (Pair a) where
    foldr f x (Pair _ y) = f y x

instance Traversable (Pair a) where
    traverse f (Pair x y) = Pair x <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

pairTrigger = undefined :: Pair (Int, Int, [Int]) (Int, Int, [Int])

---

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
    foldMap f (Big _ x y) = f x <> f y

instance Traversable (Big a) where
    traverse f (Big x y z) = liftA2 (Big x) (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Big a b c

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

bigTrigger = undefined :: Big (Int, Int, [Int]) (Int, Int, [Int])

---

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger w x y z) = Bigger w (f x) (f y) (f z)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ x y z) = f x <> f y <> f z

instance Traversable (Bigger a) where
    traverse f (Bigger w x y z) = liftA3 (Bigger w) (f x) (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Bigger a b c d

instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

biggerTrigger = undefined :: Bigger (Int, Int, [Int]) (Int, Int, [Int])

---

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n, Arbitrary (n a) , Arbitrary a ) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance Functor n => Functor (S n) where
    fmap f (S x y) = S (fmap f x) (f y)

instance Foldable n => Foldable (S n) where
    foldMap f (S nx y) = (foldMap f nx) <> f y

instance Traversable n => Traversable (S n) where
    traverse f (S x y) = S <$> (traverse f x) <*> (f y)

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq

sTrigger = undefined :: S [] (Int, Int, [Int])

---

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty          = Empty
    fmap f (Leaf x)       = Leaf $ f x
    fmap f (Node xs y zs) = Node (fmap f xs) (f y) (fmap f zs)

instance Foldable Tree where
    foldMap _ Empty          = mempty
    foldMap f (Leaf x)       = f x
    foldMap f (Node xs y zs) = foldMap f xs <> f y <> foldMap f zs

instance Traversable Tree where
    traverse _ Empty          = pure Empty
    traverse f (Leaf x)       = Leaf <$> f x
    traverse f (Node xs y zs) = (pure Node) <*> (traverse f xs) <*> (f y) <*> (traverse f zs)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = oneof [
        return Empty
        , Leaf <$> arbitrary
        , liftA3 Node arbitrary arbitrary arbitrary ]

instance Eq a => EqProp (Tree a) where (=-=) = eq

treeTrigger = undefined :: Tree (Int, Int, [Int])


main = do
    quickBatch (functor treeTrigger)
    quickBatch (traversable treeTrigger)
    quickBatch (functor sTrigger)
    quickBatch (traversable sTrigger)
    quickBatch (functor biggerTrigger)
    quickBatch (traversable biggerTrigger)
    quickBatch (functor bigTrigger)
    quickBatch (traversable bigTrigger)
    quickBatch (functor pairTrigger)
    quickBatch (traversable pairTrigger)
    quickBatch (functor threeTrigger)
    quickBatch (traversable threeTrigger)
    quickBatch (functor listTrigger)
    quickBatch (traversable listTrigger)
    quickBatch (functor optionalTrigger)
    quickBatch (traversable optionalTrigger)
    quickBatch (functor constantTrigger)
    quickBatch (traversable constantTrigger)
    quickBatch (functor identityTrigger)
    quickBatch (traversable identityTrigger)
