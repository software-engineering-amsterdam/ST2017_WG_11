--Setup from lab notes

module Lab1Answers where
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

--Workshop exercise 1 as a test (Working) to learn the language a bit took about 2 hours

f1, f2 :: Int -> Int
f1 = \ n -> sum [0..n]
f2 = \ n -> (n*(n+1)) `div` 2

test1 = quickCheckResult (\n -> n>=0 --> f1 n == f2 n)

--2 hrs spent, notes added in notes file

--Workshop exercise 2 (Task 1)

f21, f22  :: Int -> Int
f21 = \ n -> sum(map (^2) [1..n])
f22 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

test2 = quickCheckResult (\n -> n>=0 --> f21 n == f22 n)

--15 minutes spent. Notes added in notes file.

--Workshop exercise 3 (Task 1)

f31, f32 :: Int -> Int
f31 = \ n -> sum(map (^3) [1..n])
f32 = \ n -> (n*(n+1) `div` 2)^2

test3 = quickCheckResult (\n -> n>=0 --> f31 n == f32 n)

--5 minutes spent.

--Workshop exercise 4 (Task 2)

f4 :: Integer -> Bool
f4 = \ n ->  2^(length [1..n]) == length (subsequences [1..n])

test4 = quickCheckResult (f4)

--1hr spent, unsure if correct, notes updated

--Workshop exercise 5 (Task 3)

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

--Not completed - 30 mins spent so far
  
--Task 4

reversal :: Integer -> Integer
reversal = read . reverse . show

--if we reverse a reversed number it should give us the starting number.
--To test:

testreverse = \ n -> prime n --> reversal ( reversal n) == n

testrev = quickCheckResult (testreverse)

--problem solution

primelist = takeWhile(<10000)(filter(prime.reversal)primes)

--1hr, notes updated

--Task 5

--sum 101 numbers, check if sum is prime, if not remove head and try again.



--So far : 2hrs
--Task 6

solution :: Integer --[ x | x <- l, p x ]	keep elements (matching) 
solution = ( product xs + 1 | xs <- [take n primes | n <- [2..] ], not (prime (product ps + 1 )) --xs is a list of all primes from primes list from 2 up to n, keep elements resulting from the product of ps+1 that are not prime 




--Not working

--Task 7




