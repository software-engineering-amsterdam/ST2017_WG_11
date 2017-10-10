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
testFP :: Int -> [Integer] -> IO ()
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

--how many times a carmichael number is registered as prime?
testCar :: Integer -> Integer -> Integer -> IO Integer
testCar 0 _ c = do return c
testCar n nb c = do
                    test <- primeTestsF 100 nb
                    if test then
                        testCar (n-1) nb (c+1)
                     else 
                        testCar (n-1) nb c

--if applied enough times, even the first number of this list will pass Fermat's primality check

--ex6
--primeMR 3 (head $ take 1 carmichael)
testMR :: Integer -> Integer -> Integer -> IO Integer
testMR 0 _ c = do return c
testMR n nb c = do
                    test <- primeMR 10 nb
                    if test then
                        testCar (n-1) nb (c+1)
                     else 
                        testCar (n-1) nb c