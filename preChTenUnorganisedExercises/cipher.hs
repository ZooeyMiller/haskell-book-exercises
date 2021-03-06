module Cipher where

import           Data.Char
import           System.Exit                    ( exitSuccess )
import           Text.Read


baseCaesar :: (Int -> Int -> Int) -> Int -> String -> String
baseCaesar inf shift str = map (chr . go . ord) str
 where
  go 32 = 32
  go x  = (+ 96) . flip mod 26 . (flip (-) 96) . (inf shift) $ x

caesar :: Int -> String -> String
caesar = baseCaesar (+)

uncaesar :: Int -> String -> String
uncaesar = baseCaesar (flip (-))

main :: IO ()
main = do
  putStr "write a string to be ciphered: "
  term <- getLine
  putStr "write an integer to use as a cipher: "
  ciph <- getLine
  putStrLn
    $  "your ciphered term is: "
    ++ (caesar (read ciph :: Int) (fmap toLower term))
  exitSuccess
