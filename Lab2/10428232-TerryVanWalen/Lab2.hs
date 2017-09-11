module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck



infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-- Exercise 1 : 45 minutes
-- Tested with 1.000.000 and the proportions are as expected.
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

divideInQuartets :: Int -> IO [Int]
divideInQuartets l = do
  x <- probs l
  let q1 = length (filter (<0.25) x)
  let q2 = length (filter (<0.50) x) - q1
  let q3 = length (filter (<0.75) x) - (q1 + q2)
  let q4 = length (filter (<1.0) x) - (q1 + q2 + q3)
  let q = [q1, q2 , q3, q4]
  return q



-- Excercise 2 : ??
