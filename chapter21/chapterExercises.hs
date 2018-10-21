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


main = do
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
