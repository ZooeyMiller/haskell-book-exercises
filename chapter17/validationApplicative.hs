data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure x) = Failure x
    fmap f (Success x) = Success $ f x

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Failure x) (Failure y) = Failure $ x `mappend` y
    (<*>) (Failure x) _           = (Failure x)
    (<*>) _ (Failure x)           = (Failure x)
    (<*>) (Success f) (Success x) = Success $ f x

