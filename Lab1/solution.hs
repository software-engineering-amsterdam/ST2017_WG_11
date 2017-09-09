module Solution where
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


-- Assignment exercise 1
-- Workshop Exercise 2
e21, e22 :: Int -> Int
e21 = \ n -> sum (map (^2) [0..n])
e22 = \ n -> product [n, n+1,2*n + 1] `div` 6
assignment1_1 = quickCheckResult (\ n -> n >= 0 --> e21 n == e22 n)

-- Workshop Exercise 3
e31, e32 :: Int -> Int
e31 = \ n -> sum (map (^3) [0..n])
e32 = \ n -> (^2) (n * ( n + 1) `div` 2)
assignment1_2 = quickCheckResult (\ n -> n >= 0 --> e31 n == e32 n)

-- Assignment exercise 2
-- Workshop Exercise 4
e41, e42 :: Int -> Int
e41 n = length (subsequences [1..n])
e42 a  = 2 ^ a
assignment2 = quickCheckResult (\ n -> n > 0 && n <= 10 --> e41 n == e42 n)

--The complexity grows exponentially with the size of the input list, so it is hard
--to test for large lists. Without limiting the size of the input list, the program runs
--indefinetly. Even for a size of maximum 32, it can take minutes to test it.

--In fact, we are checking a mathematical function (2^n) against the implementation
--of the subsequences function, which returns the same results that we need (2^n).
--A fortunate coincidence, because it satisfies only part of the specification.
--A better approach would be to write a function which actually generates all the permutations
--of a given list and then test it against the mathematical formula (2^n)



-- Assignment exercise 3
-- Workshop Exercise 5
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
  where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

e51, e52 :: Int -> Int
e51 = \ n -> length (perms [1..n])
e52 = \ n -> product [1..n]

assignment3 = quickCheckResult (\ n  -> (n >= 0 && n < 6) --> e51 n == e52 n)
--The property is very hard to test, again. The complexity of the algorithm is in factorial time.
--and due to the recursive implementation of the factorial number, stack overflow occurs
--almost immediately.
--The bottleneck is caused by the permutations function

--In this exercise we are only testing if perms satisfies a part of its specification. We do this by testing a small subset with quickcheck but we can not prove the mathematical fact because we are not testing every possibility. On paper we can prove the mathematical fact by induction and we also did this in the workshop.




-- Assignment exercise 4
-- Workshop Exercise 5
reversal :: Integer -> Integer
reversal = read . reverse . show

hasReversedPrime :: Integer -> Bool
hasReversedPrime = \n -> prime n && (reversal n) > n && prime (reversal n)

loop :: [Integer] -> [Integer]
loop [] = []
loop (x:xs) = x : (reversal x) : loop xs

f4 :: [Integer]
f4 = loop (filter hasReversedPrime [2..10000])
assignment4 = f4

--The main idea is to avoid the computation of the prime algorithm four times for two numbers
--13 and 31, and then 31 and 13
--Secondly, all prime numbers are safe to be reversed multiple times because the number can't be a multiple of 10.

--In this example we are curious about how the Haskell language processes this in a lazy way. Is it necessary to avoid calculating the same prime or does Haskell automatically avoids this?


-- Assignment exercise 5
assignment5 = sum(f5 101 primes)

f5 :: Int -> [Integer] -> [Integer]
f5 p xs     | prime(sum sublist) = sublist
            | otherwise = f5 p (tail xs)
            where sublist = take p xs

-- For this implementation we rely on the primes function which we used earlier and on the built-in sum function
-- To test this we have to make sure that the prime function outputs an inclusive ordered list of primes starting with the smallest prime. We also have to test that our function isn't skipping primes and we can test this by outputting all first integers in the sublists that did not add up to a prime and these have to be the same as the list of primes before our starting prime. We know that this is the smallest possible solution because we go recursively through a sorted solution. This guarantees that first solution found is also the smallest.



-- Assignment exercise 6
assignment6 = f6 2 primes
assignment6all = fall 2 primes

f6 :: Int -> [Integer] -> [Integer]
f6 p xs | prime((product sublist) + 1) = f6 (p+1) xs
        | otherwise = sublist
        where sublist = take (p) xs

fall :: Int -> [Integer] -> [[Integer]]
fall p xs | prime((product sublist) + 1) = fall (p+1) xs
           | otherwise = sublist : fall (p + 1) xs
        where sublist = take (p) xs




-- Assignment execise 7
--ex7
--Luhn Algorithm 1h30min
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
isMaster n = (first2 == [5,1]
              || first2 == [5,2]
              || first2 == [5,3]
              || first2 == [5,4]
              || first2 == [5,5])
              &&
             (length listR) == 16 &&
             algorithm listR
         where listR = intToList n
               first2 = take 2 (reverse listR)
               listLength = length listR

isVisa :: Integer -> Bool
isVisa n = (head (reverse listR)) == 4 && (length listR) == 16 && algorithm listR
           where listR = intToList n

--as for testing, I can't find a way to involve quickcheck into this process, so I will employ some simple tests on some valid card numbers for each type. Also, I make sure the
--card number is of only one type

--valid card numbers
testingcardnumbers = [4532920348852300, 5318850055871739, 340593225932303]
visatestcase      = filter isVisa testingcardnumbers
mastertestcase    = filter isMaster testingcardnumbers
americantestcase  = filter isAmericanExpress testingcardnumbers

assignment7 = length visatestcase == 1 && length mastertestcase == 1 && length americantestcase == 1
