module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String 
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWork _ = "ERROR"

digits :: Int -> [Int] 
digits n = go n [] 10
  where go int list divisor
         | int == 0 = list
         | otherwise = go a ([b] ++ list) divisor
             where (a, b) = divMod int divisor

wordNumber :: Int -> String 
wordNumber = concat . intersperse "-" . map digitToWord . digits