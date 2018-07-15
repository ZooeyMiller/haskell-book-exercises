import           Data.Int


-- Cardinality = 258

data NumberOrBool =
        Numba Int8
        | BoolyBool Bool
        deriving (Eq, Show)

myNumba = Numba (-128)
