module Cipher where

import           Data.Char

-- the basic logic is taken from the Caesar cipher written in an earlier chapter
-- it only works with lower case letters but, yolo
mapChar :: (Int -> Int -> Int) -> Int -> Char -> Char
mapChar inf shift char = (chr . go . ord) char
    where
        go 32 = 32
        go x  = (+96) . flip mod 26 . (flip (-) 96) . (inf shift) $ x


zipWithCipherString :: String -> String -> [(Int, Char)]
zipWithCipherString string cipher = go string cipher cipher []
        where
            go [] _ _ result = result
            go string [] initialCipher result =
                go string initialCipher initialCipher result
            go (' ':xs) cipher initialCipher result =
                go xs cipher initialCipher (result ++ [(0, ' ')])
            go (x:xs) (y:ys) initialCipher result =
                go xs ys initialCipher (result ++ [((flip (-) 97) . ord $ y, x)])

vigenere :: String -> String -> String
vigenere = ((map . uncurry $ mapChar (+)) .) . zipWithCipherString
