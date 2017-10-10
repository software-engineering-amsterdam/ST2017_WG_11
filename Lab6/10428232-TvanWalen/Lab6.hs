module Lab6 where
import Data.List
import System.Random
import Test.QuickCheck
import Numeric
import Data.Char

import Lecture6

-- Exercise 1 - time spent: 2 hours
-- based on https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation

-- -- decToBin function used from: https://stackoverflow.com/a/1959734
-- decToBin x = reverse $ decToBin' x
--   where
--     decToBin' 0 = []
--     decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a
--
--
-- -- Convert power y to base 2 and put in reversed list.
-- -- Loop over list, if 1 than use output else just calculate power for next function
-- exM' :: Integer -> Integer -> Integer -> Integer
-- exM' x y n = fex x n (reverse (decToBin y)) x
--
-- fex :: Integer -> Integer ->  [Integer] -> Integer -> Integer
-- fex x n [] t = 1
-- fex x n (k:ks) t | k == 1 = multM t (fex x n ks tn) n
--                  | otherwise = fex x n ks tn
--   where tn = multM t t n





-- Exercise 2 -- 10 minutes
-- With larger exponents there is a clear time saving as shown for the following calculations

-- *Lab6> exM' 234567 8910111 12131415 ==> 400068 | (0.00 secs, 108,072 bytes)
-- *Lab6> expM 234567 8910111 12131415 ==> 400068 | (1.42 secs, 59,654,784 bytes)

-- *Lab6> exM' 234567 89101111 12131415 ==> 288243 | (0.00 secs, 114,024 bytes)
-- *Lab6> expM 234567 89101111 12131415 ==> 288243 | (16.45 secs, 562,987,800 bytes)



-- Exercise 3 -- 10 minutes
-- A composite number is a number that is not prime.
composites :: [Integer]
composites = filter (\x -> not (prime x)) [2..]



-- Exercise 4 -- 1.45 hours
-- Increasing k increases the number of candidates tested and thus the probability that the results are true (less false positives).
-- as shown by executing with differnt k's. It identifies lower composites more accurately as not prime.

-- Results
-- N: 100, k: 1, Min: 9,  Rest: [9,9,9,9,9,9,9,9,9,9]                        | (0.05 secs, 21,829,512 bytes)
-- N: 100, k: 2, Min: 9,  Rest: [9,15,15,15,15,15,15,21,21,25]               | (1.12 secs, 591,383,376 bytes)
-- N: 100, k: 3, Min: 65, Rest: [65,65,85,85,91,91,91,91,91,91]              | (2.53 secs, 1,396,726,120 bytes)
-- N: 100, k: 4, Min: 91, Rest: [91,91,435,561,561,561,561,561,561,561]      | (9.55 secs, 5,447,005,392 bytes)
-- N: 100, k: 5, Min: 65, Rest: [65,91,561,561,561,561,1105,1105,1105,1105]  | (17.48 secs, 10,199,674,544 bytes)


-- Nice output for report
test4kloop :: Int -> IO ()
rest4kloop 0 = do putStrLn ("")
test4kloop k = do
  let n = 100
  response <- test4loop k composites n
  let r = sort response
  putStrLn ("N: " ++ show n ++ " k: " ++ show k ++ " Min: " ++ show (take 1 r) ++ " Rest: " ++ show (take 10 r))

-- Test one k and specify tries.
test4 :: Int -> Int -> IO ()
test4 k n = do
  response <- test4loop k composites n
  putStrLn ("Ordered list of composite numbers that fooled the check in " ++ show n ++ " tries:\n" ++ show (take 10 (sort response)))

test4loop :: Int -> [Integer] -> Int -> IO [Integer]
test4loop k l 0 = do return []
test4loop k l n = do
  l1 <- testTest k l
  l2 <- test4loop k l (n-1)
  return (l1 : l2)

testTest :: Int -> [Integer] -> IO Integer
testTest k [] = do return 0
testTest k (n:ns) = do
  b <- primeTestsF k n
  if b
    then do return (n)
    else testTest k ns



-- Exercise 5 - 15 minutes
-- More explanation
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
     k <- [2..],
     prime (6*k+1),
     prime (12*k+1),
     prime (18*k+1) ]


test5 :: Int -> Int -> IO ()
test5 k n = do
  response <- test4loop k carmichael n
  putStrLn ("Ordered list of carmichael numbers that fooled the check in " ++ show n ++ " tries:\n" ++ show (take 10 (sort response)))



-- Exercise 6 - 45 minutes
-- Ordered list of carmichael numbers that fooled the MR check in 10 tries:
-- k = 1, [0,294409,294409,294409,294409,56052361,216821881,216821881,2301745249,9624742921] | (0.03 secs, 5,808,352 bytes)
-- k = 2, [0,0,0,0,0,0,0,0,0,216821881]                                                      | (0.05 secs, 15,426,568 bytes)
-- k = 3, [0,0,0,0,0,0,0,0,0,0]                                                              | (0.05 secs, 17,830,288 bytes)

-- So this function gives less false posities. Especially if we use a bigger k.

test6 :: Int -> Int -> IO ()
test6 k n = do
  response <- test6loop k (take n carmichael) n
  putStrLn ("Ordered list of carmichael numbers that fooled the MR check in " ++ show n ++ " tries:\n" ++ show (take 10 (sort response)))

test6loop :: Int -> [Integer] -> Int -> IO [Integer]
test6loop k l 0 = do return []
test6loop k l n = do
  l1 <- test6Test k l
  l2 <- test6loop k l (n-1)
  return (l1 : l2)

test6Test :: Int -> [Integer] -> IO Integer
test6Test k [] = do return 0
test6Test k (n:ns) = do
  b <- primeMR k n
  if b
    then do return (n)
    else test6Test k ns


-- Exercise 6 - 2 : Time spent: 20 minutes
-- Doesnt make a mistake in the first 23 Mersenne Numbers
test62 :: Int -> [Integer] -> IO ()
test62 k [] = do putStr ("")
test62 k (p:ps) = do
  let n = (2^p - 1)
  b <- primeMR k n
  if b
    then do putStrLn ("Possible Mersenne Number for: 2^" ++ show p ++ " - 1")
    else do putStr ("")
  test62 k ps
