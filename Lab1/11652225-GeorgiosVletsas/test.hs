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


luhn :: Integer -> Bool
luhn acc = (test(digits acc) + ((test(digits acc)*9) `mod` 10 )) `mod` 10 == 0

digits :: Integer -> [Integer]
digits = map (read . (:[])) . show

summedDig :: Integer -> Integer
summedDig y = if y > 9 then (y `mod` 10)+(y `div` 10) else y


doublelist :: [Integer] -> [Integer]
doublelist [] = []
doublelist (x:[]) = []
doublelist (x:(y:xs)) = x:y*2: doublelist xs


sumNo :: [Integer] -> [Integer]
sumNo [] = []
sumNo (x:[]) = []
sumNo (x:y:xs) = x: summedDig y : sumNo xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs


test :: [Integer] -> Integer
test c = sumDigits(sumNo(doublelist c))








