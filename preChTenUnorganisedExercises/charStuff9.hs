import Data.Char

capitaliseFirst :: String -> String
capitaliseFirst "" = ""
capitaliseFirst (x : xs) = toUpper x : xs

capitaliseAll :: String -> String
capitaliseAll str = go str ""
    where go str res 
            | str == "" = res
            | otherwise = go xs (res ++ [toUpper x])
                where (x:xs) = str

getFirstUpper :: String -> Char
getFirstUpper str = toUpper (head str)

getFirstUpperComposed :: String -> Char
getFirstUpperComposed = toUpper . head