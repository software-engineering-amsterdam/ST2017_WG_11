module Lab6 where
import Data.List
import System.Random
import Test.QuickCheck
import Numeric
import Data.Char

import Lecture6

-- Exercise 1 - time spent: 2 hours
-- Commented because ambiguous.
-- -- based on https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation
-- decToBin :: Integer -> [Integer]
-- decToBin n = reverse $ decToBin' n
--     where decToBin' 0 = []
--           decToBin' n = (mod n 2:(decToBin' $ div n 2))
--
-- -- Convert power y to base 2 and put in reversed list.
-- -- Loop over list, if 1 than use output else just calculate power for next function
-- exMM :: Integer -> Integer -> Integer -> Integer
-- exMM x y n = fex x n (reverse (decToBin y)) x
--
-- fex :: Integer -> Integer ->  [Integer] -> Integer -> Integer
-- fex x n [] t = 1
-- fex x n (k:ks) t | k == 1 = multM t (fex x n ks tn) n
--                  | otherwise = fex x n ks tn
--   where tn = multM t t n


-- Exercise 2 - time spent: 10 minutes
-- When running manual tests, once it reaches large enough numbers, the running time of the
-- inefficient implementation starts to increase, while for the efficient it stays almost the same.

-- *Lab6> exM' 258134 9343523 18904524 ==> 12575528 | (0.00 secs, 108,072 bytes)
-- *Lab6> expM 258134 9343523 18904524 ==> 12575528 | (1.42 secs, 59,654,784 bytes)

-- *Lab6> exM' 258134 93435233 18904524 ==> 509360 | (0.00 secs, 114,024 bytes)
-- *Lab6> expM 258134 93435233 18904524 ==> 509360 | (16.45 secs, 562,987,800 bytes)


-- Exercise 3 - time spent: 10 minutes
-- A composite number is a number that is not prime.
composites :: [Integer]
composites = filter (\x -> not (prime x)) [2..]


-- Exercise 4 - time spent: 1 hour
-- Increasing k increases the number of candidates tested and thus the probability that the results are true (less false positives).
-- as shown by executing with differnt k's. It identifies lower composites more accurately as not prime.

-- Results
-- N: 100, k: 1, Min: 9,  Rest: [9,9,9,9,9,9,9,9,9,9]                        | (0.05 secs, 21,829,512 bytes)
-- N: 100, k: 2, Min: 9,  Rest: [9,15,15,15,15,15,15,21,21,25]               | (1.12 secs, 591,383,376 bytes)
-- N: 100, k: 3, Min: 65, Rest: [65,65,85,85,91,91,91,91,91,91]              | (2.53 secs, 1,396,726,120 bytes)
-- N: 100, k: 4, Min: 91, Rest: [91,91,435,561,561,561,561,561,561,561]      | (9.55 secs, 5,447,005,392 bytes)
-- N: 100, k: 5, Min: 65, Rest: [65,91,561,561,561,561,1105,1105,1105,1105]  | (17.48 secs, 10,199,674,544 bytes)

test4 :: Integer -> [Integer] -> IO ()
test4 _ []  = putStrLn "check worked on all numbers"
test4 k (n:ns) = do
                    cond <- primeTestsF k n
                    if cond then
                      putStrLn ("number " ++ show n ++ " fooled the algorithm!")
                    else
                      test4 k ns



-- Exercise 5 -- 45 minutes
--Carmichael numbers are special numbers which satisfy Fermat's little theorem, even though they are not prime
--They are composite numbers which are square-free (no duplicate factors) and must have three positive factors
--We ran a number of tests to check whether carmichael numbers indeed fool Fermat's little theorem
--By increasing k and using primeTestsF, we've discovered that the results are inconsistent.
--The definition says that the numbers should always fool the test, but in practice they don't
--With a k of 100 only large enough numbers are able to fool it

--if applied enough times, even the first number of this list will pass Fermat's primality check


carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

--how many times a carmichael number is registered as prime?
test5 :: Integer -> Integer -> Integer -> IO ()
test5 n nb k = do
  r <- testGeneric primeTestsF n nb k 0
  putStrLn (show nb ++ " is found "++show r++ " out of "++show n++" times to be a prime.")

--generic test function (also used in 6)
testGeneric :: (Integer -> Integer -> IO Bool) -> Integer -> Integer -> Integer -> Integer -> IO Integer
testGeneric test 0 _ _ c = do return c
testGeneric test n nb k c = do
                        res <- test k nb
                        if res then
                          testGeneric test (n-1) nb k (c+1)
                        else
                          testGeneric test (n-1) nb k c



-- Exercise 6 - 1,5 hours
--Miller
test6 :: Integer -> Integer -> Integer -> IO ()
test6 n nb k = do
  r <- testGeneric primeMR n nb k 0
  putStrLn (show nb ++ " is found "++show r++ " out of "++show n++" times to be a prime.")

--find large Mersenne numbers?
--complexity increases a lot, in 10 minutes it finds the 24th Mersenne number (19937)
--comparing results with the wikipedia list, the algorithm finds the correct Mersenne numbers, even for a small k
findLM :: Integer -> [Integer] -> IO ()
findLM _ []     = do putStrLn ("end")
findLM k (n:ns) = do
                    let nr = (2^n - 1)
                    cond <- primeMR k nr
                    if cond then
                      putStrLn ("Mersenne: " ++ show n)
                    else
                      putStr ""
                    findLM k ns
