module Cipher where 
    
import Data.Char

baseCaesar :: (Int -> Int -> Int) -> Int -> String -> String
baseCaesar inf shift str = map (chr . (+96) . flip mod 26 . (flip (-) 96) . (inf shift) . ord) str

caesar :: Int -> String -> String 
caesar = baseCaesar (+)

uncaesar :: Int -> String -> String
uncaesar = baseCaesar (flip (-))

