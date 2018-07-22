{-# LANGUAGE ScopedTypeVariables #-}

import           Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

replaceTheBleh :: String -> String
replaceTheBleh =  intercalate " " . map (getThe . notThe) . words
    where
        getThe (Just x) = x
        getThe Nothing  = "a"

replaceThe :: String -> String
replaceThe = replace "the" "a"

replace :: String -> String -> String -> String
replace toReplace replaceWith str = go [] (words str) toReplace replaceWith
        where
            go res [] _ _        = unwords res
            go res (x:xs) rep wi = if x == rep then go (res ++ [wi]) xs rep wi else go (res ++ [x]) xs rep wi
