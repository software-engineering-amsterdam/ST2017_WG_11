module Lab1 where

import Prelude hiding (reverse)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.List
import Data.Char
import System.Random

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1) 
  return (p:ps)
  
-- Question 1 - Useful logic notation
-- Time spent: 30min

probsTest :: Int -> IO [Int]
probsTest 0 = return []
probsTest n = do
    l <- probs n
    let x1 = filter (\n -> n < 0.25) l
    let x2 = filter (\n -> n >= 0.25 && n < 0.5) l
    let x3 = filter (\n -> n >= 0.5 && n < 0.75) l
    let x4 = filter (\n -> n >= 0.75 && n < 1) l
    return [length x1, length x2, length x3, length x4]

-- Test result report for n == 10000: 
-- [2498,2551,2417,2534]
-- [2490,2528,2490,2492]
-- [2473,2507,2543,2477]
-- [2545,2455,2497,2503]
-- The result show that the implementation is correct.

-- Question 2 - Recognizing triangles
-- Time spent: 1h


data Shape = NoTriangle | Equilateral 
  | Isosceles  | Rectangular | Other deriving (Eq,Show)
  
triangle :: Integer -> Integer -> Integer -> Shape

triangle a b c =
   if a == b && b == c                                      then Equilateral
   else if (^2) a + (^2) b == (^2) c                        then Rectangular  
   else if (^2) b + (^2) c == (^2) a                        then Rectangular  
   else if (^2) c + (^2) a == (^2) b                        then Rectangular  
   else if a <= 0 || b <= 0 || c <= 0                       then NoTriangle
   else if (a + b) <= c || (a + c) <= b || (c + b) <= a     then NoTriangle
   else if (a == b) || (b == c) || (c == a)                 then Isosceles
   else Other

-- Test for correctness: probably generate a lot of examples and manually check each example? Or calculate the
-- Consize test report: 


-- Question 3 - Testing properties strength
-- Time spent: 3h

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

-- Question 3a, properties of Workshop Exercise 3

a, b, c :: Int -> Bool
a x = even x && x > 3
b x = even x || x > 3
c x = (even x && x > 3) || even x

q3a, q3b, q3c, q3d :: Bool
q3a = stronger [(-10)..10] a even
q3b = stronger [(-10)..10] b even
q3c = stronger [(-10)..10] c even
q3d = stronger [(-10)..10] even c

-- Question 3b, descending list of properties

aFn = FunctionName "a" a
bFn = FunctionName "b" b
cFn = FunctionName "c" c
dFn = FunctionName "d" c

data FunctionName a b = FunctionName {
      fnName :: String,
      fn :: (a->Bool)
}

orderProperties p q | (stronger [(-10)..10] a b) = GT
                    | (weaker [(-10)..10] a b) = LT
                    | otherwise = EQ
                    where a = (fn p)
                          b = (fn q)
                
myOrdering = [ (fnName x) | x <- reverse (sortBy orderProperties [aFn, bFn, cFn, dFn])]

-- myOrdering = ["a","c","d","b"]

-- Question 4 - Recognizing Permutations
-- Time spent: 2.5h

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] a  = False
isPermutation a [] = False   
isPermutation (x:xs) y = elem x y && isPermutation xs (removeFromList x y)

-- There should be no duplicates in the permutation list, so this function is save to use (it will however remove duplicates from the list, if there are any)
removeFromList :: Eq a  => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (y:ys)
           | x == y    = removeFromList x ys
           | otherwise = y : removeFromList x ys

--exampleList :: [Int]
--exampleList = return [1,2,3]
--
--examplePermutationList :: [[Int]]
--examplePermutationList = return [[1,2,3],[2,3,1],[3,2,1]]

-- Testable list

-- Provide an ordered list of properties by strength using the weakear and stronger definition

-- Only numbers
-- Same length
-- Must be a list

pNumbers, pSameLength :: [a]->[a]->Bool
pSameLength a b = length a == length b
pNumbers a b = True

--
--pIsList :: [a] -> [a] -> Bool
--pIsList [Int] [Int] = True
--pIsList a b = False


-- Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.


-- Question 5 - Recognizing and generating derangements
-- Time spent:

index :: Eq a => a -> [a] -> Int
index n (x:xs) | n == x  = 0
               | otherwise = 1 + index n xs

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] y  = False
isDerangement y [] = False   
isDerangement (x:xs) (y:ys) =
   elem x (y:ys)  && (index x (x:xs) /= index x (y:ys)) && isDerangement xs (removeFromList x (y:ys))


deran :: [Int] -> [Int]
deran [] = []
deran a = a


-- Question 6 - Implementing and testing ROT13 encoding
-- Time spent:



-- Question 7 - Implementing and testing IBAN validation
-- Time spent:








