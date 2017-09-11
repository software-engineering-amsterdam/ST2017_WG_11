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
data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z   | x <= 0
                   || y <= 0
                   || z <= 0                    = NoTriangle
                 | x >= y + z
                   || y >= x + z
                   || z >= x + y                = NoTriangle
                 | x == y
                   && x == z
                   && y == z                    = Equilateral
                 | x^2 + y^2 == z^2
                   || x^2 + z^2 == y^2
                   || y^2 + z^2 == x^2          = Rectangular
                 | x == y
                   || x == z
                   || y == z                    = Isosceles
                 | otherwise                    = Other
