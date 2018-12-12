{-# LANGUAGE ScopedTypeVariables #-}

import           Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

replaceTheBleh :: String -> String
replaceTheBleh = intercalate " " . map (getThe . notThe) . words
 where
  getThe (Just x) = x
  getThe Nothing  = "a"

replaceThe :: String -> String
replaceThe = replace "the" "a"

replace :: String -> String -> String -> String
replace toReplace replaceWith str = go [] (words str) toReplace replaceWith
 where
  go res [] _ _ = unwords res
  go res (x : xs) rep wi =
    if x == rep then go (res ++ [wi]) xs rep wi else go (res ++ [x]) xs rep wi

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go 0 (words str) False
 where
  go res [] _ = res
  go res (x : xs) lastWasThe =
    go (res + (pass lastWasThe (head x))) xs (x == "the")
  pass lastWasThe char = if (lastWasThe && isVowel char) then 1 else 0



isVowel :: Char -> Bool
isVowel = flip elem $ "aeiou"

countVowels :: String -> Int
countVowels = length . filter (&& True) . map isVowel
