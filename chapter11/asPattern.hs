{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool

isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf t@(x : xs) (y : ys) =
  if x == y then isSubseqOf xs ys else isSubseqOf t ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map go . words where go t@(x : xs) = (t, toUpper x : xs)
