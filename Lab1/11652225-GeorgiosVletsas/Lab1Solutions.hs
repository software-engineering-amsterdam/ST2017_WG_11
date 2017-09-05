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

--[ x | x <- l, p x ]	keep elements (matching) - from http://rigaux.org/language-study/syntax-across-languages-per-language/Haskell.html

solution :: Integer -> [Integer] -> Integer
--solution = ( product xs + 1 | xs <- [take n primes | n <- [2..] ], not (prime (product xs + 1 )) --xs is a list of all primes from primes list from 2 up to n, keep elements resulting from the product of ps+1 that are not prime 




--Not working

--Task 7

{-| use luhn to write functions isAmericanExpress, isMaster, isVisa :: Integer -> Bool for checking whether an input number is a valid American Express Card, Master Card, or Visa Card number. Consult Wikipedia for the relevant properties.{
1. From the rightmost digit, which is the check digit, and moving left, double the value of every second digit. If the result of this doubling operation is greater than 9 (e.g., 8 × 2 = 16), 
then add the digits of the product (e.g., 16: 1 + 6 = 7, 18: 1 + 8 = 9) or alternatively subtract 9 from the product (e.g., 16: 16 - 9 = 7, 18: 18 - 9 = 9).
2. Take the sum of all the digits.
3. If the total modulo 10 is equal to 0 (if the total ends in zero) then the number is valid according to the Luhn formula; else it is not valid.

-}

luhn :: Integer -> Bool
isAmericanExpress, isMaster, isVisa :: Integer -> Bool


DoubleEveryTwo :: [Integer] -> [Integer]
DoubleEveryTwo [] = []
DoubleEveryTwo [x:[]] = []
DoubleEveryTwo (x:y:xs) = x: 2*y : DoubleEveryTwo xs

-- we can find the first digit of the number by using n mod 10, and the 2nd digit with n div 10. This works because the product of doubling one number can never be a triple digit number.
-- n `mod` 10,  n `div` 10

sumNo :: Integer -> Integer






















