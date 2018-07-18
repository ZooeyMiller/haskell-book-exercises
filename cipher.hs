module Cipher where

import           Data.Char

baseCaesar :: (Int -> Int -> Int) -> Int -> String -> String
baseCaesar inf shift str = map (chr . go . ord) str
    where
        go 32 = 32
        go x  = (+96) . flip mod 26 . (flip (-) 96) . (inf shift) $ x

caesar :: Int -> String -> String
caesar = baseCaesar (+)

uncaesar :: Int -> String -> String
uncaesar = baseCaesar (flip (-))

