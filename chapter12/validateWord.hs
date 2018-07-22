newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = if c >= v then Just (Word' str) else Nothing
    where
        v = length $ filter (&&True) $ map (flip elem $ vowels) str
        c = length $ filter (&&True) $ map (not . flip elem vowels) str
