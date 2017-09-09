--

module Lab1Answers where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Global function

power :: Int -> [Int] -> [Int]
power p [] = []
power p (x:xs) = (x^p) : (power p xs)



--Exercise 1:
-- For this exercise I used the answers on the course page. When implementing I did not understand what \n did but could guess it was some sort of variable assignment that I needed because the test would not work without this. After reading The Haskell Road to logic I learned that this are lambda functions. My question is how do lambda functions differ from just providing a parameter to the function?
f21, f22 :: Int -> Int
f21 \n = \n -> sum (power 2 [0..n])
f22 \n = \n -> div (product [n, n+1,2*n + 1]) 6
test11 = quickCheckResult (\n -> n >= 0 --> f21 n == f22 n)

f31, f32 :: Int -> Int
f31 = \n -> sum (power 3 [0..n])
f32 = \n -> (div (product [n, n+1]) 2)^2
test12 = quickCheckResult (\n -> n >= 0 --> f31 n == f32 n)



--Exercise 2:
-- This property is hard to test because it takes quite a while to calculate the subsequences. On normal hardware (personal laptop) it is impossible to test this for big numbers.
-- It is impossible to prove the mathematical fact in this manner, it would take forever, but we could disprove it if we find a counterexample and at the same time prove the implementation to be valid.
-- So actually we're testing our implementation by generating a testcase of enough examples so we can reasonably assume our implementation to be correct. Of course we have to seperately prove the mathematical correctness and we can do this by induction.
f41, f42 :: Int -> Int
f41 = \n -> length (subsequences [1..n])
f42 = \n -> 2^(length [1..n])
test2 = quickCheckResult (\n -> n >= 0 && n < 10 --> f41 n == f42 n)



--Exercise 3:
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- Still takes a lot of time to calculate the permutations so we can only use a small subset.
-- Still testing the implementation. If it works for the small subset and we can prove the mathematics by induction it should be fine.
f51, f52 :: Int -> Int
f51 = \n -> length (perms [1..n])
f52 = \n -> product [1..n]
test3 = quickCheckResult (\n -> n >= 0 && n < 10 --> f51 n == f52 n)


reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..10000]


-- In this implementation I wanted to make sure that we do not recalculate primes. The first step was to create a list of primes that have a reversed prime but to not save this reversed prime. The second step is to loop through this list and add the reverse of the primes in there. Because we already tested if they were a prime we don't have to do this again.
-- You can test this by comparing all the primes in the complete list of primes to each other but this could take a while so you probably would do it on a small subsample.
hasReversedPrime :: Integer -> Bool
hasReversedPrime = \n -> prime n && (reversal n) > n && prime (reversal n)

loop :: [Integer] -> [Integer]
loop [] = []
loop (x:xs) = x : (reversal x) : loop xs

f4 :: [Integer]
f4 = loop (filter hasReversedPrime [2..10000])
test4 = f4


-- Exercise 5
-- Worked together with Nicolae on this problem to find the where solution. My first solution did not include this and therefore contained the same line of code twice.
-- This solution repeatedly uses the full list of primes previously generated except that it trims of the first integer. To calculate it creates a sublist of a the first p number of integers.
-- To test if the consecutive primes indeed add op to another prime you can simple test this in another function. To test if it is the smallest would be pointless because we already loop through all the possibilities.
-- But you can check if the primes are indeed really consecutive and a mistake wasn't made in that part of the code.
test5 = sum(f5 101 primes)

f5 :: Int -> [Integer] -> [Integer]
f5 p (x:xs) | prime(sum sublist) = sublist
            | otherwise = f5 p xs
            where sublist = (take p (x:xs))


-- Exercise 6
-- This exercise uses most of the previous solution but switches the lines. and instead of dropping the first integer it adds the next prime to the list.
-- If we find a counterexample the conjecture is refuted. This means that after we find one counterexample we are done.
test6 = f6 2 primes

f6 :: Int -> [Integer] -> [Integer]
f6 p xs | prime((product sublist) + 1) = f6 (p+1) xs
        | otherwise = sublist
        where sublist = take (p) xs




-- -- -- Exercise 7
luhn :: Integer -> Bool
luhn d = mod (sum (doubleEverySecond (toDigitsAndReverse d))) 10 == 0

toDigitsAndReverse :: Integer -> [Integer]
toDigitsAndReverse d | d < 10 = [d]
                     | otherwise = mod d 10 : toDigitsAndReverse (div d 10)

doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond [x] = [x]
doubleEverySecond (x:y:zs) = x : (addDigitsIfGreater9 (y*2)) : doubleEverySecond zs

addDigitsIfGreater9 :: Integer -> Integer
addDigitsIfGreater9 d | d > 9 = d - 9
                      | otherwise = d

-- To know the bank we can check the first few digits and the number of digits
isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress d | integerStartWith d [34, 37]
                      && numDigits d == 15
                      && luhn d
                      = True
                    | otherwise = False

isMaster d          | integerStartWith d (mergeLists [51..55] [222100..272099])
                      && numDigits d == 16
                      && luhn d
                      = True
                    | otherwise = False

isVisa d            | integerStartWith d [4]
                      && elem (numDigits d) [13, 16, 19]
                      && luhn d
                      = True
                    | otherwise = False

-- Only need it for positive numbers
numDigits :: Integer -> Integer
numDigits d | d == 0    = 1
            | d > 0     = 1 + truncate (logBase 10 (fromIntegral d))
            | d < 0     = error "Not a positive integer"
            | otherwise = error "NumDigits otherwise error"


-- MergeLists retrieved from: https://stackoverflow.com/a/3938449/8102945
mergeLists :: [a] -> [a] -> [a]
mergeLists xs     []     = xs
mergeLists []     ys     = ys
mergeLists (x:xs) (y:ys) = x : y : mergeLists xs ys


integersStartSame :: Integer -> Integer -> Bool
integersStartSame d n | d > n && div d divider == n = True
                      | otherwise = False
                      where divider = 10^((numDigits d) - (numDigits n))

integerStartWith :: Integer -> [Integer] -> Bool
integerStartWith i []     = False
integerStartWith i (d:ds) | integersStartSame i d = True
                          | otherwise             = integerStartWith i ds

-- Testcases based on: https://www.freeformatter.com/credit-card-number-generator-validator.html
-- If only one cardnumber is returned than it is valid
testingcardnumbers = [4716010268081706, 2221006169315674, 340593225932303]
visacase      = filter isVisa testingcardnumbers
mastercase    = filter isMaster testingcardnumbers
americancase  = filter isAmericanExpress testingcardnumbers


-- Exercise 8:
data Boy = Matthew | Peter | Jack | Arnold | Carl
          deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew Matthew = False
accuses Matthew Carl = False

accusers :: Boy -> [Boy]
accusers b1 =

guilty, honest :: [Boy]
