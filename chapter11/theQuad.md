```hs
data Quad = One
    | Two
    | Three
    | Four
    deriving (Eq, Show)
```

how many different forms can this take?

1.

```hs
eQuad :: Either Quad Quad
```

4 \* 2 = 8

2.

```hs
prodQuad :: (Quad, Quad)
```

4 ^ 2 = 16

3.

```hs
funcQuad :: Quad -> Quad
```

4 ^ 4 = 256

4.

```hs
prodTBool :: (Bool, Bool, Bool)
```

2 ^ 3 = 8

5.

```hs
gTwo :: Bool -> Bool -> Bool
```

(2 ^ 2) ^ 2 = 16

(I got stressed out with the verification as I was still thinking in the wrong way, so wrote it out )

```
1 True False = True
2 False True = True
3 True True = True
4 False False = True

1 True False = True
2 False True = True
3 True True = True
4 False False = False

1 True False = True
2 False True = True
3 True True = False
4 False False = False

1 True False = True
2 False True = False
3 True True = False
4 False False = False

1 True False = False
2 False True = False
3 True True = False
4 False False = False

1 True False = False
2 False True = False
3 True True = False
4 False False = True

1 True False = False
2 False True = False
3 True True = True
4 False False = True

1 True False = False
2 False True = True
3 True True = True
4 False False = True

1 True False = True
2 False True = False
3 True True = True
4 False False = True

1 True False = True
2 False True = True
3 True True = False
4 False False = True

1 True False = True
2 False True = False
3 True True = False
4 False False = True

1 True False = False
2 False True = True
3 True True = False
4 False False = False

1 True False = False
2 False True = False
3 True True = True
4 False False = False

1 True False = False
2 False True = True
3 True True = True
4 False False = False

1 True False = True
2 False True = False
3 True True = True
4 False False = False

1 True False = False
2 False True = True
3 True True = False
4 False False = True
```

```hs
fTwo :: Bool -> Quad -> Quad
```

(4 ^ 4) ^ 2 = 65536
