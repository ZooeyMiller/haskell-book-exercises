given

```haskell
(.) :: ( b -> c ) -> ( a -> b ) -> a -> c
-- fmap fmap
fmap :: Functor f => ( m -> n ) -> f m -> f n
fmap :: Functor g => ( x -> y ) -> g x -> g y
```

(.) f g = \x -> f (g x)

fmap . fmap == \x -> fmap (fmap x)

so in `fmap . fmap` in the sig for `(.)`

b = m -> n
c = f m -> f n
a = ( x -> y )
b = g x -> g y

so

`b = m -> n` is compatible with `b = g x -> g y` so we know
that

m = g x
n = g y

so

b = g x -> g y
c = f (g x) -> f (g y)
a = ( x -> y )

so `fmap . fmap :: (Functor f, Functor g) => ( x -> y ) -> f (g x) -> f (g y)`
