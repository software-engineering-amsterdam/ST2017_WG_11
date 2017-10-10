module Lab6 where
import Data.List
import System.Random
import Test.QuickCheck
import Numeric
import Data.Char

import Lecture6

--ex1, time spent: 2h

--code is in the lecture file

-- decToBin :: Integer -> [Integer]
-- decToBin n = reverse $ decToBin' n
--     where decToBin' 0 = []
--           decToBin' n = (mod n 2:(decToBin' $ div n 2))


-- exMM :: Integer -> Integer -> Integer -> Integer
-- exMM x y n = exM' x (reverse $ decToBin y) n x
--     where exM' _ [] _ _ = 1
--           exM' x (h:hs) n p | h==1 = multM p (exM' x hs n val) n
--                             | otherwise = exM' x hs n val
--                             where val = multM p p n

--ex2, time spent:
--Report. 
--When running manual tests, once it reaches large enough numbers, the running time of the
--inefficient implementation starts to increase, while for the efficient it stays almost the same.


--ex3
--composite numbers are numbers which have divisors apart from one and the number itself -> not prime
composites :: [Integer]
composites = filter (not.prime) [2..]

--ex4
--when increasing k the least number which is able to fool the algorithm also decreases
testFP :: Integer -> [Integer] -> IO ()
testFP _ []  = putStrLn "check worked on all numbers"
testFP k (n:ns) = do
                    cond <- primeTestsF k n
                    if cond then
                      putStrLn ("number " ++ show n ++ " fooled the algorithm!")
                    else 
                      testFP k ns

--ex5
--Carmichael numbers are special numbers which satisfy Fermat's little theorem, even though they are not prime
--They are composite numbers which are square-free (no duplicate factors) and must have three positive factors
--We ran a number of tests to check whether carmichael numbers indeed fool Fermat's little theorem
--By increasing k and using primeTestsF, we've discovered that the results are inconsistent.
--The definition says that the numbers should always fool the test, but in practice they don't
--With a k of 100 only large enough numbers are able to fool it

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]     

--generic test function
testG :: (Integer -> Integer -> IO Bool) -> Integer -> Integer -> Integer -> Integer -> IO Integer
testG test 0 _ _ c = do return c
testG test n nb k c = do
                        res <- test k nb
                        if res then
                          testG test (n-1) nb k (c+1)
                        else 
                          testG test (n-1) nb k c

--how many times a carmichael number is registered as prime?
testCar :: Integer -> Integer -> Integer -> IO Integer
testCar n nb k = testG primeTestsF n nb k 0

--if applied enough times, even the first number of this list will pass Fermat's primality check

--ex6
--Miller
testMR :: Integer -> Integer -> Integer -> IO Integer
testMR n nb k = testG primeMR n nb k 0

--find large Mersenne numbers?
--complexity increases a lot, in 10 minutes it finds the 24th Mersenne number (19937)
--comparing results with the wikipedia list, the algorithm finds the correct Mersenne numbers, even for a small k
findLM :: Integer -> [Integer] -> IO ()
findLM _ []  = putStrLn "end"
findLM k (n:ns) = do
                    let nr = (2^n - 1)
                    cond <- primeMR k nr
                    if cond then
                      putStrLn ("Mersenne: " ++ show n)
                    else 
                      putStr ""
                    findLM k ns