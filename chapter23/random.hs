module RandomExample where

import           Control.Applicative            ( liftA3 )
import           Control.Monad                  ( replicateM )
import           Control.Monad.Trans.State
import           System.Random

data Die =
    DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x


rollDieThreeTimes :: Int -> (Die, Die, Die)
rollDieThreeTimes x = do
  let s        = mkStdGen x
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _ ) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetToN :: Int -> StdGen -> (Int, [Die])
rollsToGetToN i g = go 0 (0, []) g
 where
  go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
  go sum (count, dice) gen
    | sum >= i
    = (count, dice)
    | otherwise
    = let (die, nextGen) = randomR (1, 6) gen
      in  go (sum + die) (count + 1, (intToDie die) : dice) nextGen

rollsToGetToTwenty :: StdGen -> (Int, [Die])
rollsToGetToTwenty = rollsToGetToN 20
