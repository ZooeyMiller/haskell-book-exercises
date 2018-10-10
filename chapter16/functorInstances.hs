import           Test.QuickCheck
import           Test.QuickCheck.Function


functorIdentity :: ( Functor f, Eq (f a) ) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: ( Eq (f c), Functor f ) => ( a -> b ) -> ( b -> c ) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)


functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = pairGen

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x $ f y

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = twoGen

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

threeeGen :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threeeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = threeeGen

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c $ f d

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = fourGen

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c $ f d

fourrGen :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourrGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = fourrGen

data Trivial = Trivial

-- 8. Cannot make instance Functor for type Trivial as Functor accepts a kind of * -> *
-- and Trivial has a kind of *. More practically Functor is a way of operating on lifted
-- values whilst maintaining structure, where Trivial lifts no values. It is not only impossible
-- to create Functor for Trivial, it is also pointless.

main :: IO ()
main = do
    quickCheck (functorIdentity :: Identity Int -> Bool)
    quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)
    quickCheck (functorIdentity :: Two Int Int -> Bool)
    quickCheck (functorCompose' :: Two Int Int -> IntToInt -> IntToInt -> Bool)
    quickCheck (functorIdentity :: Three Int Int Int -> Bool)
    quickCheck (functorCompose' :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)
    quickCheck (functorIdentity :: Three' Int Int -> Bool)
    quickCheck (functorCompose' :: Three' Int Int -> IntToInt -> IntToInt -> Bool)
    quickCheck (functorIdentity :: Four Char Int Int Int -> Bool)
    quickCheck (functorCompose' :: Four Char Int Int Int -> IntToInt -> IntToInt -> Bool)
    quickCheck (functorIdentity :: Four' Int Int -> Bool)
    quickCheck (functorCompose' :: Four' Int Int -> IntToInt -> IntToInt -> Bool)
