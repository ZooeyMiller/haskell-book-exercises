{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Char
import           Data.List


type DaPhone = [(Char, [Char])]

phone :: DaPhone
phone = [
    ('1', ['1']),
    ('2', ['a', 'b', 'c', '2']),
    ('3', ['d', 'e', 'f', '3']),
    ('4', ['g', 'h', 'i', '4']),
    ('5', ['j', 'k', 'l', '5']),
    ('6', ['m', 'n', '0', '6']),
    ('7', ['p', 'q', 'r', 's', '7']),
    ('8', ['t', 'u', 'v', '8']),
    ('9', ['w', 'x', 'y', 'z', '9']),
    ('0', ['+', '_','0']),
    ('#', ['.', ',', '#'])
    ]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
    ]

-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone char = list ++ [findCharLower phone char]
    where
        upperChar = isUpper char
        char' = if upperChar then toLower char else char
        list = if upperChar then [('*', 1)] else []


findCharLower :: DaPhone -> Char -> (Digit, Presses)
findCharLower phone char = return
    where
        press = find (\(_, l) -> elem char l) phone
        presses = fmap (\(_, xs) -> getPresses char xs) press
        safePresses = case presses of
            Just x  -> x
            Nothing -> 0
        safeKey = case press of
            Just (x, _) -> x
            Nothing     -> '1'
        return = (safeKey, safePresses)


getPresses :: Char -> [Char] -> Int
getPresses char chars = go chars 1
   where
        go [] _       = 0
        go (x:xs) num = if x == char then num else go xs (num + 1)

getAllPresses :: DaPhone -> [Char] -> [(Digit, Presses)]
getAllPresses phone string = concat $ map (reverseTaps phone) string

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) num -> num + p) 0

mostPopularElement :: Eq a => a -> [a] -> a
mostPopularElement def str = fst $ foldr (\t@(_, amt) t2@(_, amt2) -> if amt > amt2 then t else t2) (def, 0) chars
    where
        chars = nub $ map (\char -> (char, getTotalInstances str char)) str

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopularElement 'a'

getTotalInstances :: Eq a => [a] -> a -> Int
getTotalInstances xs x = length $ filter (==x) xs

costOfMostPopularLetter :: DaPhone -> String -> Presses
costOfMostPopularLetter phone string = totalTaps
    where
        mostPop = mostPopularLetter string
        totalInstance = getTotalInstances string mostPop
        taps = reverseTaps phone mostPop
        totalTaps = foldr (\(_, x) y -> x + y) 0 $ concat $ map (reverseTaps phone) $ replicate totalInstance mostPop


coolestLtr :: [String] -> Char
coolestLtr =  mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord xs = mostPopularElement "" $ map (map toLower) $ concat $ map words xs
