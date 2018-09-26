import           Data.List
import           Test.Hspec
import           Test.QuickCheck

import           WordNumber      (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
    describe "digitToWord" $ do
        it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"

    describe "digits" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1,0,0]

    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"


half x = x / 2

tautology :: (Eq a, Fractional a) => a -> Bool
tautology x = ((*2) $ half x) == x

testTaut :: IO ()
testTaut = quickCheck tautology

prop_half :: Double -> Bool
prop_half x = ((*2) $ half x) == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, t)       = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered . sort

prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative x y = x + y == y + x

main2 :: IO ()
main2 = do
    putStrLn "half"
    quickCheck prop_half
    putStrLn "sort"
    quickCheck (prop_listOrdered :: [Int] -> Bool)
    putStrLn "+ associative"
    quickCheck (prop_plusAssociative :: Int -> Int -> Int -> Bool)
    putStrLn "+ commutative"
    quickCheck (prop_plusCommutative :: Int -> Int -> Bool)
