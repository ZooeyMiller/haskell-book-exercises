import           Data.Monoid             hiding ( (<>) )
import           Data.Semigroup
import           Test.QuickCheck                ( Arbitrary
                                                , Gen
                                                , arbitrary
                                                , oneof
                                                , quickCheck
                                                )

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
    Identity x <> Identity y = Identity $ x <> y

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

type IdAssoc = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two z a) = Two (x <> z) (y <> a)

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = twoGen

type TwoAssoc = (Two [String] [Int]) -> (Two [String] [Int]) -> (Two [String] [Int]) -> Bool

-- I decided not to do Three and Four as they are conceptually the same as
-- Identity and Two but with more typing soz

newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
    BoolDisj False <> _ = BoolDisj False
    _ <> BoolDisj False = BoolDisj False
    _ <> _ = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj True
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = oneof [return $ BoolDisj True, return $ BoolDisj False]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    BoolConj True <> _ = BoolConj True
    _ <> BoolConj True = BoolConj True
    _ <> _ = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj False
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = oneof [return $ BoolConj True, return $ BoolConj False]


type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    Fst _ <> x = x
    x@(Snd a) <> _ = x

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Fst a, return $ Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = orGen

type OrAssoc = (Or [String] [Int]) -> (Or [String] [Int]) -> (Or [String] [Int]) -> Bool


newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine x) <> (Combine y) = Combine (\n -> (x n) <> (y n))

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ const mempty
    mappend = (<>)

newtype Comp a =
    Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
    (Comp x) <> (Comp y) = Comp $ y . x

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
    mempty = Comp $ id
    mappend = (<>)

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failure x) <> (Failure y) = Failure $ x <> y
    _ <> x@(Success _) = x
    x <> _ = x

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success x)) <> (AccumulateRight (Success y)) = AccumulateRight $ Success $ x <> y
    x@(AccumulateRight (Success _)) <> _ = x
    _ <> x = x


newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Success x)) <> (AccumulateBoth (Success y)) = AccumulateBoth $ Success $ x <> y
    (AccumulateBoth (Failure x)) <> (AccumulateBoth (Failure y)) = AccumulateBoth $ Failure $ x <> y
    _ <> x@(AccumulateBoth(Success _)) = x
    x <> _ = x


newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \x -> (mempty, x)
    mappend f g = Mem $ f'
                    where
                        f' x = (a `mappend` a', b')
                            where (a, b) = runMem g x
                                  (a', b') = runMem f b



f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

memCheck :: IO ()
memCheck = do
  let rmzero  = runMem mempty 0
      rmleft  = runMem (f' `mappend` mempty) 0
      rmright = runMem (mempty `mappend` f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (monoidLeftIdentity :: (Identity [Int]) -> Bool)
  quickCheck (monoidRightIdentity :: (Identity [Int]) -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: (Two [Int] [Char]) -> Bool)
  quickCheck (monoidRightIdentity :: (Two [Int] [Char]) -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc)





