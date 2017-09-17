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


 -- propTriangleSym :: Ord a => Integer -> Integer -> Integer -> Bool
 -- propSym a b c = triangle a b c == triangle c b a
 --              && triangle a b c == triangle b c a
 --
 -- propTriangleTrans :: Ord a => [a] -> [a] -> [a] -> Bool
 -- propTrans a b c = ((triangle a b) && (triangle b c)) --> (triangle a c)


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


-- Exercise 4 -- Recognizing Permutations -- 60 minutes
-- Solution based on: http://geekyplatypus.com/generating-permutations-and-derangements-using-haskell/
-- This is a really fast solution and works even when there are duplicates in the lists.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []     = True
isPermutation xs []     = False
isPermutation [] xs     = False
isPermutation (l:l1) l2 | length (l:l1) == length l2 = isPermutation (l1) (delete l l2)
                        | otherwise = False


propLength :: Ord a => [a] -> [a] -> Bool
propLength a b = isPermutation a b --> length a == length b

propRefl :: Ord a => [a] -> Bool
propRefl a = isPermutation a a --> null a

propSym :: Ord a => [a] -> [a] -> Bool
propSym a b = isPermutation a b == isPermutation b a

propTrans :: Ord a => [a] -> [a] -> [a] -> Bool
propTrans a b c = ((isPermutation a b) && (isPermutation b c)) --> (isPermutation a c)



-- Exercise 5 -- Derangements -- 60 minutes
-- test5 = isDerangement seed
isAllDifferent :: Eq a => [a] -> [a] -> Bool
isAllDifferent [] [] = True
isAllDifferent l1 [] = False
isAllDifferent [] l2 = False
isAllDifferent (x:xs) (y:ys) | x /= y = isAllDifferent xs ys
                             | otherwise = False

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement l1 [] = False
isDerangement [] l2 = False
isDerangement xs ys | isPermutation xs ys = isAllDifferent xs ys
                    | otherwise = False


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- Also got most of this from: http://geekyplatypus.com/generating-permutations-and-derangements-using-haskell/
-- Good and simple function, would be faster to implement it without the perms function but this makes it a lot easier.
deran :: Integer -> [[Integer]]
deran n = filter (\p -> isDerangement p [0..(n-1)]) (perms [0..(n-1)])


propDeranLength :: Ord a => [a] -> [a] -> Bool
propDeranLength a b = isDerangement a b --> length a == length b

propDeranPerm :: Ord a => [a] -> [a] -> Bool
propDeranPerm a b = isDerangement a b --> isPermutation a b

-- Only the empty list is a derangement of itself
propDeranRefl :: Ord a => [a] -> Bool
propDeranRefl a = isDerangement a a --> null a

propDeranSym :: Ord a => [a] -> [a] -> Bool
propDeranSym a b = isDerangement a b == isDerangement b a

propDeranTrans :: Ord a => [a] -> [a] -> [a] -> Bool
propDeranTrans a b c = ((isDerangement a b) && (isDerangement b c)) --> (isDerangement a c)




-- Exercise 6 -- 60 minutes
-- Wanted to do somtehing differnt than using modulus.
alphabet = (init [(chr 0)..'a']) ++ ['a'..'z'] ++ ['a'..'z']
rot13 :: [Char] -> [Char]
rot13 [] = ""
rot13 (x:xs) = take 1 (drop ((ord x) + 13) alphabet) ++ rot13 xs


-- Exercise 7 -- 2 hours
testcases = ["NL39RABO0300065264", "LI21088100002324013AA", "PK36SCBL0000001123456702"]
iban :: String -> Bool
iban s = valid s && mod (read (char2digitsString (move4end s)) :: Integer) 97 == 1

valid :: String -> Bool
valid s = validString s && validNLIBAN s

validString :: String -> Bool
validString [] = True
validString (x:xs) = elem x (['A'..'Z'] ++ ['0'..'9']) && validString xs

validNLIBAN :: String -> Bool
validNLIBAN s = take 2 s == "NL"
                && validNLLength s
                && length (filter (\p -> elem p ['0'..'9']) s) >= 10

validNLLength :: String -> Bool
validNLLength s = length s == 18

move4end :: String -> String
move4end s = (drop 4 s) ++ first4 where first4 = take 4 s

char2digitsString :: [Char] -> [Char]
char2digitsString [] = ""
char2digitsString (x:xs) | ord x < ord 'A' = x : char2digitsString xs
                         | otherwise       = show (ord x - ord 'A' + 10) ++ char2digitsString xs

testIbans = filter iban testcases
