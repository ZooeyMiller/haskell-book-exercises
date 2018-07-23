lefts' :: [Either a b] -> [a]
lefts' = foldr go []
    where
        go (Right _) res = res
        go (Left x) res  = x : res

rights' :: [Either a b] -> [b]
rights' = foldr go []
    where
        go (Right x) res = x : res
        go (Left _) res  = res

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
        where
            go (Left x) (xs, ys)  = (x:xs, ys)
            go (Right y) (xs, ys) = (xs, y:ys)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\_ -> Nothing) (Just . f) e
