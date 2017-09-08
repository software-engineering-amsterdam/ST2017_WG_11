module Lab1 where
import Data.List
import Test.QuickCheck    

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

--Ex 1 ~ 20 min
sum2 :: Int -> Int
sum2 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

sum2H :: Int -> Int
sum2H = \ n -> sum (map (^2) [0..n])

sum2V :: Int -> Int
sum2V = \ n -> sum (map (\n -> n*n) [0..n])

sum3 :: Int -> Int 
sum3 = \ n -> ((n*(n+1) `div` 2) ^ 2)

sum3H :: Int -> Int
sum3H = \ n -> sum (map (^3) [0..n])

testSum21 = quickCheckResult (\ n -> n >= 0 --> sum2V n == sum2H n)
testSum22 = quickCheckResult (\ n -> n >= 0 --> sum2 n == sum2H n)
testSum3 = quickCheckResult (\ n -> n >= 0 --> sum3 n == sum3H n)

--Ex 2 ~ 45 min
permsH :: Int -> Int
permsH n = length (subsequences [0..n])

perms :: Int -> Int
perms a = 2 ^ a

testPerms = quickCheck (\ n -> n <= 32 --> perms n == permsH n)
--The complexity grows exponentially with the size of the input list, so it is hard 
--to test for large lists. Without limiting the size of the input list, the program runs
--indefinetly. Even for a size of maximum 32, it takes minutes to test it. 

--In fact, we are checking a mathematical function (2^n) against the implementation 
--of the subsequences function, which returns the same results that we need (2^n).
--A fortunate coincidence, because it satisfies only part of the specification.
--A better approach would be to write a function which actually generates all the permutations
--of a given list and then test it against the mathematical formula (2^n)

--Ex 3
permsGiven :: [a] -> [[a]]
permsGiven [] = [[]]
permsGiven (x:xs) = concat (map (insrt x) (permsGiven xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

fac 0 = 1
fac n = n * fac(n - 1)

facP n = product [1..n]

testFactorial = quickCheck (\ n -> (n >= 0 && n <= 10) --> length (permsGiven [1..n]) == fac n)
--The property is very hard to test, again. The factorial number simply grows too quickly
--and due to the recursive implementation of the factorial number, stack overflow occurs
--almost immediately.
--The two implementations of the factorial function can generate very large numbers with
--no issues, but the bottleneck is caused by the permutations functions. 


--ex 4 -> 1h 30min
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..10000]

reversal :: Integer -> Integer
reversal = read . reverse . show

primeReversed :: [Integer]
primeReversed = primeAddReversalBack (filter valid primes)

primeAddReversalBack :: [Integer] -> [Integer]
primeAddReversalBack [] = []
primeAddReversalBack (x:xs) = x:(reversal x):primeAddReversalBack xs

valid :: Integer -> Bool
valid n = (n >= 10) && (reversal n > n) && (prime (reversal n))
--The main idea is to avoid the computation of the prime algorithm four times for two numbers
--13 and 31, and then 31 and 13
--Secondly, all prime numbers are safe to be reversed multiple times

--ex 5 - 1h30min
-- --For this implementation we rely on the primes function which we used earlier and on the built-in sum function
-- --The only thing left to test is that generate101Lists indeed generates lists of 101 elements.
get101Sum :: [Integer] -> [Integer]
get101Sum xs = performComputations xs 101

performComputations :: [Integer] -> Int -> [Integer]
performComputations xs p | prime(sum sublist) = sublist 
                         | otherwise = performComputations (tail xs) p
                         where sublist = take p xs

--ex6
--generate only the first counterexample
getFirstNonPrime :: [Integer] -> Int -> [Integer]
getFirstNonPrime xs p | prime((product sublist) + 1) = getFirstNonPrime xs (p + 1)
                      | otherwise =  sublist 
                      where sublist = take p xs

--generates all of them
getNonPrimeAll :: [Integer] -> Int -> [[Integer]]
getNonPrimeAll xs p | prime((product sublist) + 1) = getNonPrimeAll xs (p + 1)
                         | otherwise = sublist : getNonPrimeAll xs (p + 1)
                         where sublist = take p xs

--ex7
--Luhn Algorithm
luhn :: Integer -> Bool
luhn n = algorithm (intToList n)

algorithm :: [Integer] -> Bool
algorithm (check:xs) = (sum (map (\x -> if x > 9 then x - 9 else x) (multiplyOddPos xs 0)) + check) `mod` 10 == 0

multiplyOddPos :: [Integer] -> Integer -> [Integer]
multiplyOddPos [] i = []
multiplyOddPos (x:xs) i | i `mod` 2 == 1 = x:(multiplyOddPos xs (i+1))
                        | otherwise = (x*2):(multiplyOddPos xs (i+1))

intToList :: Integer -> [Integer]
intToList 0 = []
intToList n = (n `mod` 10) : intToList (n `div` 10)

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = (first2 == [3,4] || first2 == [3,7]) && (length listR) == 15 && algorithm listR 
                  where listR = intToList n
                        first2 = take 2 (reverse listR)

isMaster :: Integer -> Bool
isMaster n = (first2 == [5,1] || first2 == [5,2] || first2 == [5,3] || first2 == [5,4] || first2 == [5,5]) && 
             (length listR) == 16 && 
             algorithm listR 
         where listR = intToList n
               first2 = take 2 (reverse listR)
               listLength = length listR

isVisa :: Integer -> Bool
isVisa n = (head (reverse listR)) == 4 && (length listR) == 16 && algorithm listR 
           where listR = intToList n 