
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd


{--Task 1 -- Time Spent(Total man hours): 6 hours


1. How would we do exercise 4.14 (pp. 129). Confused on what we're looking for and how to get it.
2. Exercise 4.21 pp 132
3. What is a powerset
4. How can russel paradox be used in programming? What are its uses.

-}

--Task 2 -- Time Spent(Total man hours): 2 hours

-- First wanted to try to do it based on an empty set and add random numbers but that was harder than expected. This is a lot easier. We are wodering how this could be done without using lists?
getRandomList :: Int -> Int -> IO [Int]
getRandomList x 0 = return []
getRandomList x n = do
  r <- getRandomInt x
  l <- getRandomList x (n-1)
  return (r : l)

getRandomInt :: Int -> IO Int
getRandomInt n = randomRIO (1,n) :: IO Int

randomSet :: Int -> Int -> IO (Set Int)
randomSet x n = do
  r <- getRandomInt n
  l <- getRandomList x r
  let  s = list2set l
  return s


--Task 3 -- Time Spent(Total man hours): 5hrs 30 mins

--Run main to see test results
main = do testVisual
          putStrLn "\nQuickCheck for Union, Intersection, A-B, B-A:"
          testUnion
          testInters
          testDiffA
          testDiffB

--Intersection
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = list2set (a `intersect` b)

--Union
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set (a `union` b)

--Difference
-- https://stackoverflow.com/questions/4573692/haskell-lists-difference
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = list2set (a \\ b)

--Declare properties to check if the operators are true

--the A U B == ((A-B)U(B-A)) U (A^B)
unionProp :: Ord a => Set a -> Set a -> Bool
unionProp a b = setUnion a b == setUnion ( setUnion (setDifference a b) (setDifference b a) ) (setIntersection a b)

--A^B == (A U B) - ((A-B)U(B-A))
interProp :: Ord a => Set a -> Set a -> Bool
interProp a b = setIntersection a b == setDifference ( setUnion a b ) (setUnion (setDifference a b) (setDifference b a))

--(A U B ) - (B - A) == A
diffAProp :: Ord a => Set a -> Set a -> Bool
diffAProp a b = setDifference (setUnion a b) (setDifference b a) == a

--(A U B ) - (A - B) == B
diffBProp :: Ord a => Set a -> Set a -> Bool
diffBProp a b = setDifference (setUnion a b) (setDifference a b) == b

--test with random sets
testVisual = do a <- randomSet 8 9
                b <- randomSet 8 9
                putStrLn "\n({Set A},{set B}):"
                print (a, b)
                putStrLn "\n(A U B):"
                print (setUnion a b)
                print (unionProp a b)
                putStrLn "\n(A ^ B):"
                print (setIntersection a b)
                print (interProp a b)
                putStrLn "\n(A - B):"
                print (setDifference a b)
                print (diffAProp a b)
                putStrLn "\n(B - A):"
                print (setDifference b a)
                print (diffBProp a b)


--Unable to understand arbitrary, so used QuickCheck to generate lists which then got changed to sets
--Union quickcheck property
unionPropQuickCheck :: [Integer] -> [Integer] -> Bool
unionPropQuickCheck a b = setUnion x y == setUnion ( setUnion (setDifference x y) (setDifference y x) ) (setIntersection x y)
                          where x = list2set a
                                y = list2set b

--Intersection quickcheck property
interPropQuickCheck :: [Integer] -> [Integer] -> Bool
interPropQuickCheck a b = setIntersection x y == setDifference ( setUnion x y ) (setUnion (setDifference y x) (setDifference x y))
                          where x = list2set a
                                y = list2set b

--difference quickCheck property (A U B ) - (B - A) == A
diffAPropQuickCheck :: [Integer] -> [Integer] -> Bool
diffAPropQuickCheck a b = setDifference (setUnion x y) (setDifference y x) == x
                         where x = list2set a
                               y = list2set b

--difference quickCheck property (A U B ) - (A - B) == B
diffBPropQuickCheck :: [Integer] -> [Integer] -> Bool
diffBPropQuickCheck a b = setDifference (setUnion x y) (setDifference x y) == y
                         where x = list2set a
                               y = list2set b



testUnion = quickCheck unionPropQuickCheck
-- +++ OK, passed 100 tests.
testInters = quickCheck interPropQuickCheck
-- +++ OK, passed 100 tests.
testDiffA = quickCheck diffAPropQuickCheck
-- +++ OK, passed 100 tests.
testDiffB = quickCheck diffBPropQuickCheck
-- +++ OK, passed 100 tests.

{--Task 4 --Time spent: 3hrs

1. What exactly does xRx mean? Relation of x to x?
2. Is an empty set considered reflexive and irreflective to itself?
3. I do understand the notion of a transitive closure, but the definition R+ = Un>=1 R^n (page 172) is unclear to me.
4. At Definition 5.75, the definition of a equivelence class is given, where they mention that R is the R-equivalence class of a, or the equivalence class of a modulo R. However, why is it the equivalence class of modulo R?
5.

-}


-- Exercise 8
-- Time spent: 1.5h

-- There is a difference between the symmetric closure of the transitive closure and the transitive closure of the symmetric closure. Here is an counterexample:
-- Let rel  = [(1,2),(2,3)]
-- trClos (symClos rel) == symClos ( trClos rel) => False
-- trClos (symClos rel) = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
-- symClos ( trClos rel) = [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
-- Both outcomes don't contain duplicated, and the first outcome is longer then the second. Thus there is a difference.
