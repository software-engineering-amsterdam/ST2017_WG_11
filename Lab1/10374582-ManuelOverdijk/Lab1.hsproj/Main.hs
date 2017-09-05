module Lab1 where

import Prelude hiding (reverse)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.List


-- Lab1, Manuel Overdijk (10374582)

-- Exercise 1
-- Time spent: 1h

quickCheck' prop = output <$> quickCheckWithResult stdArgs{chatty = True} prop

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes
   
primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 -->  
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
 
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Workshop Exercise 2

e21, e22 :: Int -> Int
e21 = \ n -> sum (map (^2) [0..n])
e22 = \ n -> (n * (n+1) * (2*n+1)) `div` 6
test1 = quickCheck' (\ n -> n >= 0 --> e21 n == e22 n)

-- Workshop Exercise 3

e31, e32 :: Int -> Int
e31 = \ n -> sum (map (^3) [0..n])
e32 = \ n -> (^2) (n * ( n + 1) `div` 2)  
test2 = quickCheck' (\ n -> n >= 0 --> e31 n == e32 n)

-- Exercise 2
-- Time spent: 0

-- subsequences :: [a] -> [[a]]
 
e41, e42 :: [a] -> Int
e41 = \ l -> length l
e42 = \ l -> length (subsequences l)
 
test3 = quickCheck' (\ n -> n >= 0 --> e42 [1..n] == (2^(e41 [1..n])))

-- Testing the property for integer lists of the form [1..n].
-- TODO explenations


-- Exercise 3
-- Time spent: 0

-- Workshop Exercise 5

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

e51, e52 :: Int -> Int
e51 = \ n -> length (perms [1..n])
e52 = \ n ->  factorial n

test5 = quickCheck' (\ n  -> (n >= 0 && n < 6) --> e51 n == e52 n)
-- TODO explenations

-- Exercise 4
-- Time spent: 1.5h

reversal :: Integer -> Integer
reversal = read . reverse . show

isPrimeReversed :: Integer -> Bool
isPrimeReversed = \n -> prime n && prime (reversal n)

reversePrimes :: [Integer]
reversePrimes = 11 : filter isPrimeReversed [13..10000] 

-- How would you test this function, by the way?

-- Exercise 5
-- Time spent: 2h

isPrimeSumRecursive :: Int -> [Integer] -> [Integer]
isPrimeSumRecursive n (x:xs)  | prime (sum (take n (x:xs))) = take n (x:xs)
                              | otherwise = isPrimeSumRecursive n xs    
                                            
e101 = isPrimeSumRecursive 101 primes
smallestPrime = sum e101

-- Do you have to test that your answer is correct? How could this be checked?

-- Exercise 6
-- Time spent: 1h

nonPrimes :: [[Integer]]
nonPrimes = [ls | ls <- [ take p primes | p <- [2..]], not $ prime (calcPrime ls)]

calcPrime :: [Integer] -> Integer
calcPrime a = (product a) + 1

-- Smalles counter example:
smallestNonPrime = calcPrime (head nonPrimes)

-- Exercise 7
-- Source (with changes): https://stackoverflow.com/a/3963628/4230326
toDigits :: Integer -> [Integer]
toDigits n = map (\x -> read [x] :: Integer) (show n)

luhn :: Integer -> Bool
luhn n = algorithm (toDigits (reversal n))

algorithm :: [Integer] -> Bool
algorithm (x:xs) = (sum (iterate2nd (xs)) + x) `mod` 10 == 0

iterate2nd :: [Integer] -> [Integer]
iterate2nd [] = []
iterate2nd [x] = [x]
iterate2nd (x:y:xs) = x : luhnCalc y : iterate2nd xs


luhnCalc :: Integer -> Integer
luhnCalc n | (n * 2) > 9 = (n * 2) - 9
           | otherwise = n

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = False
isMaster n = False
isVisa n = False








