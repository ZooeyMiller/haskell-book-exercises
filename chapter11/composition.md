say i have a function `x :: (a -> b -> c)` and a function `y :: (c -> d)`

my compose operator `(.)` only composes unary functions - `((b -> c) -> (a -> b) -> (a -> c))`

so if I compose functions `x` and `y` with the composition operator like so

`y . x` - what I want is a function `(a -> b -> d)` but what I get is a type error (or at the point of the resulting function being applied to arguments I do)

because of autocurrying we can remember that type signatures are sort of parenthetised

i.e. the sigs of `x` and `y` become `(a -> (b -> c))` and `(c -> d)` respectively

as such when I run `y . x` what I am actually doing is passing the function returned from

`x arbritrayValue` to `y`, rather than fully applying arguments to x before passing the result on to y, and y doesn't take a function, which in this case, is what it is being applied to

If however, I suffix `y` with its own composition operator as thus

`((y .) . x)` then `y` is actually getting composed with two functions, which allows

for the binary function `x` to be composed with the unary function `y`, having x be fully applied

before `y` is applied to the result

for reference - `(y .) . :: (a -> b -> c) -> a -> b -> d` where `y . :: (a -> c) -> a -> d`
