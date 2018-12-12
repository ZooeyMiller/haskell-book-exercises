myFromTo :: (Enum a, Ord a) => a -> a -> [a]
myFromTo start stop = go start stop []
 where
  go a b list | a > b     = list
              | a == b    = list ++ [b]
              | otherwise = go (succ a) (b) (list ++ [a])

eftBool :: Bool -> Bool -> [Bool]
eftBool False _ = [False, True]
eftBool _     _ = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myFromTo

eftInt :: Int -> Int -> [Int]
eftInt = myFromTo

eftChar :: Char -> Char -> [Char]
eftChar = myFromTo
