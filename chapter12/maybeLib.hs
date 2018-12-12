isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee fb _ Nothing  = fb
mayybee _  f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee $ id

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr con []
 where
  con Nothing  res = res
  con (Just x) res = x : res

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = go ms []
 where
  go []              res = Just res
  go (Nothing  : _ ) _   = Nothing
  go ((Just x) : xs) res = go xs (res ++ [x])

