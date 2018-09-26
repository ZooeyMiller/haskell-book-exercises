import           Data.Monoid
import           Test.QuickCheck

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

newtype First' a =
    First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' Nada) x = x
    mappend x (First' Nada) = x
    mappend x y             = x

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = frequency [
        (1, return $ First' Nada),
        (1, only) ]
        where only = do
                     a <- arbitrary
                     return $ First' $ Only a


firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String
                    -> First' String
                    -> First' String
                    -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
