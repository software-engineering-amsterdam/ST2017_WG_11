module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

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



-- Excercise 2 : 30 minutes
-- To test this we could generate a list of triples and filter them with the triangle function.
-- In this list we could check some outputs but I see no way how to test this.
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



-- Exercise 3 : 4 hours
-- There are only 4 individual properties
p1, p2, p3, p4 :: Integer -> Bool
p1 = \n -> even n && n > 3
p2 = \n -> even n
p3 = \n -> even n || n > 3
p4 = \n -> (even n && n > 3) || even n

testP12 = stronger [(-10)..10] p1 p2
testP32 = stronger [(-10)..10] p3 p2
testP42 = stronger [(-10)..10] p4 p2
testP24 = stronger [(-10)..10] p2 p4

-- Quicksort the list
test3 = quicksort' [fp1, fp2, fp3, fp4]

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

quicksort' :: [FunctionPair a b] -> [String]
quicksort' [] = []
quicksort' (x:xs) =
  quicksort' [y | y <- xs, (stronger [(-10)..10] (prop y) (prop x))]
  ++ [(name x)]
  ++ quicksort' [y | y <- xs, not (stronger [(-10)..10] (prop y) (prop x))]

-- Nicolae told me about functionpairs. This took a long time to figure out.
data FunctionPair a b = FunctionPair {
    name :: String
    , prop :: (Integer->Bool)
}

fp1 = FunctionPair "p1" p1
fp2 = FunctionPair "p2" p2
fp3 = FunctionPair "p3" p3
fp4 = FunctionPair "p4" p4


-- descendingStrengthList
-- sortBy stronger plist
