module Reverse where 
rvrs :: String -> String
rvrs c = drop 9 c ++ " " ++ drop 6 (take 8 c) ++ " "  ++ take 5 c
main :: IO ()
main = print $ rvrs "Curry is awesome"
