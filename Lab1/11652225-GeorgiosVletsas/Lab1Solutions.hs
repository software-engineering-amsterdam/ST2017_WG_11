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

--solution :: Integer -> [Integer] -> Integer
--solution = ( product xs + 1 | xs <- [take n primes | n <- [2..] ], not (prime (product xs + 1 )) --xs is a list of all primes from primes list from 2 up to n, keep elements resulting from the product of ps+1 that are not prime 




--Not working

--Task 7

{-| use luhn to write functions isAmericanExpress, isMaster, isVisa :: Integer -> Bool for checking whether an input number is a valid American Express Card, Master Card, or Visa Card number. Consult Wikipedia for the relevant properties.{

-}


luhn :: Integer -> Bool
luhn acc = (threeStepResult(digits acc)) `mod` 10 == 0

--make integer into list
digits :: Integer -> [Integer]
digits = map (read . (:[])) . show

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs

--condition to add the double digits, returns individual digits added if y>9
-- we can find the first digit of the number by using n mod 10, and the 2nd digit with n div 10. This works because the product of doubling one number can never be a triple digit number.
-- n `mod` 10,  n `div` 10
doubleCalc :: Integer -> Integer
doubleCalc y = if y > 9 then (y `mod` 10)+(y `div` 10) else y

--doubles every second number
doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond (x:[]) = [x]
doubleEverySecond (x:(y:xs)) = if (odd (intListLength(x:(y:xs)))) then (x:y*2: doubleEverySecond xs) else (x*2:y: doubleEverySecond xs)

--step 2, sums all double digit numbers
sumDoubles :: [Integer] -> [Integer]
sumDoubles [] = []
sumDoubles (x:[]) = [x]
sumDoubles (x:y:xs) = if (odd (intListLength(x:(y:xs)))) then (x: doubleCalc y : sumDoubles xs) else (doubleCalc x: y : sumDoubles xs)

--step 3, sums all digits of the acc number.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

--combines the 3 steps in one function, giving us the sum of all digits of the account
threeStepResult :: [Integer] -> Integer
threeStepResult c = sumDigits(sumDoubles(doubleEverySecond c))


--Time spent for working luhn implementation: 3hr and 30 minutes


isVisa :: Integer -> Bool
isVisa acc = (head cardNo == 4) && (intListLength cardNo == 16) && (luhn acc)
         where cardNo = digits acc 

isMaster :: Integer -> Bool
isMaster acc = (take 2 cardNo == [5,1] || take 2 cardNo == [5,2] || take 2 cardNo == [5,3] || take 2 cardNo == [5,4] || take 2 cardNo == [5,5] ) && (intListLength cardNo == 16) && (luhn acc)
         where cardNo = digits acc


isAmericanExpress :: Integer -> Bool
isAmericanExpress acc = (take 2 cardNo == [3,4] || take 2 cardNo == [3,7]) && (intListLength cardNo == 15) && (luhn acc)
         where cardNo = digits acc

--Additional time spent for cards: 30 minutes


