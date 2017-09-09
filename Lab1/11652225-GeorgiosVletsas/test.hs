module TestingDoc where

import Data.List
import Test.QuickCheck

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

------------------------------------------------------------------------------------------ TESTING DOCUMENT TO TEST CODE PARTS BEFORE IMPLEMENTING


--Task 5

--sum 101 numbers, check if sum is prime, if not remove head and try again.



checkSum :: [Integer] -> Integer
checkSum [] = 0
checkSum [x] = 0
checkSum (x:xs) 
   | sum ( take 101 (x:xs)) == prime    = 1
   | otherwise                          = checkSum xs



