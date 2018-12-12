fibs = 1 : scanl (+) 1 fibs

first20Fibs = take 20 fibs

fibsLessThan100 = takeWhile (< 100) fibs

factorialRec 0 = 1
factorialRec n = n * (factorialRec (n - 1))

fact = scanl (*) 1 [2 ..]
