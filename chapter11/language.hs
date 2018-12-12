{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Char
import           Data.List


capitalizeWord :: String -> String
capitalizeWord (x : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph y = go [] (words y) False
 where
  go res []       _     = (concat . intersperse " ") res
  go []  (x : xs) _     = go [capitalizeWord x] xs (checkLastWord x)
  go res (x : xs) True  = go (res ++ [capitalizeWord x]) xs (checkLastWord x)
  go res (x : xs) False = go (res ++ [x]) xs (checkLastWord x)
  checkLastWord = (== '.') . head . reverse
