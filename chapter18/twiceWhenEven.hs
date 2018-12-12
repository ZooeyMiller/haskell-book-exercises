twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x then [x * x, x * x] else []

-- Given twiceWhenEven [1..3] expected result is [4, 4]

twiceWhenEvenDesugared :: [Integer] -> [Integer]
twiceWhenEvenDesugared xs = do
  xs >>= (\x -> if even x then [x * x, x * x] else [])
