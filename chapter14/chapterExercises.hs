import           Data.List
import           Test.Hspec
import           Test.QuickCheck

import           WordNumber                     ( digitToWord
                                                , digits
                                                , wordNumber
                                                )

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
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"


half x = x / 2

prop_half :: Double -> Bool
prop_half x = ((* 2) $ half x) == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
 where
  go _ status@(_      , False) = status
  go y (       Nothing, t    ) = (Just y, t)
  go y (       Just x , t    ) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered . sort

prop_plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: (Num a, Eq a) => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x

prop_multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_multAssociative x y z = x * (y * z) == (z * x) * y

prop_multCommutative :: (Num a, Eq a) => a -> a -> Bool
prop_multCommutative x y = x * y == y * x

prop_quotRemProof :: Integral a => a -> a -> Bool
prop_quotRemProof _ 0 = True -- legit or nah?
prop_quotRemProof x y = (quot x y) * y + (rem x y) == x

prop_divModProof :: Integral a => a -> a -> Bool
prop_divModProof _ 0 = True
prop_divModProof x y = (div x y) * y + (mod x y) == x

prop_powerNotCommutative x y | x < 0 || y < 0 = True
                             | x == y         = True
                             | otherwise      = (x ^ y) /= (y ^ x)

prop_powerNotAssociative x y z | x < 0 || y < 0 || z < 0 = True
                               | x == y || y == z || x == z = True
                               | otherwise = (x ^ (y ^ z)) /= ((z ^ y) ^ x)

prop_doubleReverse z = (reverse $ reverse z) == id z

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
  putStrLn "* associative"
  quickCheck (prop_multAssociative :: Int -> Int -> Int -> Bool)
  putStrLn "* commutative"
  quickCheck (prop_multCommutative :: Int -> Int -> Bool)
  putStrLn "quotRemProof"
  quickCheck (prop_quotRemProof :: Int -> Int -> Bool)
  putStrLn "divModProof"
  quickCheck (prop_divModProof :: Int -> Int -> Bool)
  putStrLn "powerNotCommutative"
  quickCheck (prop_powerNotCommutative :: Int -> Int -> Bool)
  putStrLn "powerNotAssociative"
  quickCheck (prop_powerNotAssociative :: Int -> Int -> Int -> Bool)
  putStrLn "double reverse"
  quickCheck (prop_doubleReverse :: [Int] -> Bool)
