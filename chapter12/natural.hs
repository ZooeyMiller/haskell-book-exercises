data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + (natToInteger x)


integerToNat :: Integer -> Maybe Nat
integerToNat int | int < 0   = Nothing
                 | otherwise = Just (go int Zero)
 where
  go 0 res = res
  go n res = go (n - 1) (Succ res)
