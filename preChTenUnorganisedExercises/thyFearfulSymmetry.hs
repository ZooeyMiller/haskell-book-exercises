split :: Char -> String -> [String]
split splitOn string = go string []
    where go str list
           | str == [] = list
           | otherwise = go (dropWhile (==splitOn) $ dropWhile (/=splitOn) str) (list ++ [(takeWhile (/=splitOn) str)])

myWords :: String -> [String]
myWords = split ' '

myLines :: String -> [String] 
myLines = split '\n'

firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful\
\ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]


main :: IO ()
main = print $
     "Are they equal? "
      ++ show (myLines sentences == shouldEqual)
