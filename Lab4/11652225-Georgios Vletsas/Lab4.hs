module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd

--Task 2
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomList :: Int -> Int -> IO [Int]
getRandomList x 0 = return []
getRandomList x n = do
  r <- getRandomInt x
  l <- getRandomList x (n-1)
  return (r : l)


setGen :: Int -> Int -> IO (Set Int)
setGen x n = do
  r <- getRandomInt n
  l <- getRandomList x r
  let  s = list2set l
  return s



--Task 3

--Run main to see test results
main = do testVisual
          putStrLn "\nQuickCheck for Union, Intersection, A-B, B-A:"
          testUnion
          testInters
          testDiffA
          testDiffB


setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = list2set (a `intersect` b)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set (a `union` b) 

-- https://stackoverflow.com/questions/4573692/haskell-lists-difference
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = list2set (a \\ b)



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


testVisual = do a <- setGen 8 9
                b <- setGen 8 9
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

--2 hrs so far

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


--Total: 5hrs 20 mins