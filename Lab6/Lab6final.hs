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


-- Exercise 4 - time spent:
--when increasing k the least number which is able to fool the algorithm also decreases
testFP :: Int -> [Integer] -> IO ()
testFP _ []  = putStrLn "check worked on all numbers"
testFP k (n:ns) = do
                    cond <- primeTestsF k n
                    if cond then
                      putStrLn ("number " ++ show n ++ " fooled the algorithm!")
                    else
                      testFP k ns
