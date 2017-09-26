module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

--Task 3

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = list2set (a `intersect` b)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set (a `union` b) 

-- https://stackoverflow.com/questions/4573692/haskell-lists-difference
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = list2set (a \\ b)

--transform 3 integers into a set
setTransf :: Ord a => a -> a -> a -> Set a
setTransf a b c = list2set [a,b,c]

--generate a set with 3 random numbers
setGen :: IO (Set Int) 
setGen = do x <- getRandomInt 5
            y <- getRandomInt 5
            z <- getRandomInt 5
            let rSet = setTransf x y z
            return (rSet)

--the A U B == ((A-B)U(B-A)) U (A^B)
unionProp :: Ord a => Set a -> Set a -> Bool
unionProp a b = setUnion a b == setUnion ( setUnion (setDifference a b) (setDifference b a) ) (setIntersection a b)

--(A U B) - (A-B) == set B
diffProp :: Ord a => Set a -> Set a -> Bool
diffProp a b = b == setDifference ( setUnion a b ) (setDifference a b) 

--A^B == (A U B) - ((A-B)U(B-A))
interProp :: Ord a => Set a -> Set a -> Bool
interProp a b = setIntersection a b == setDifference ( setUnion a b ) (setUnion (setDifference a b) (setDifference b a)) 

test = do a <- setGen
          b <- setGen
          putStrLn "Set A and set B:"
          print (a, b)
          putStrLn "\n A U B"
          print (setUnion a b)
          print (unionProp a b)
          putStrLn "\n A ^ B"
          print (setIntersection a b)
          putStrLn "\n A - B"
          print (setDifference a b)
          print (interProp a b)
          putStrLn "\n B - A"
          print (setDifference b a)

--2 hrs so far




