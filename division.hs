data DividedResult = 
  Result (Integer, Integer)
  | DividedByZero
  deriving (Show, Eq)

myDivMod :: Integer -> Integer -> DividedResult
myDivMod x y
  | y == 0 = DividedByZero
  | otherwise  = go x y 0
    where go x y count
           | x < y = Result (count, x)
           | otherwise = go (x - y) y (count + 1)

--myDivMod 15 2

--go 15 2 0
--go 13 2 1
--go 11 2 2
--go 9 2 3
--go 7 2 4
--go 5 2 5
--go 3 2 6
--go 1 2 7 -> (7, 1)

data DividedResultOnly = 
  ResultOnly Integer
  | DividedByZero'
  deriving (Show, Eq)

myDiv :: Integer -> Integer -> DividedResultOnly
myDiv x y
  | y == 0 = DividedByZero'
  | x < 0 && y < 0 = myDiv (negate x) (negate y)
  | x < 0 = negateDividedResultOnly (myDiv (negate x) y)
  | y < 0 = negateDividedResultOnly (myDiv x (negate y))
  | otherwise = go x y 0
    where go x y count
           | x < y = ResultOnly count
           | otherwise = go (x - y) y (count + 1)

negateDividedResultOnly :: DividedResultOnly -> DividedResultOnly
negateDividedResultOnly DividedByZero' = DividedByZero'
negateDividedResultOnly (ResultOnly x) = ResultOnly (negate x)
