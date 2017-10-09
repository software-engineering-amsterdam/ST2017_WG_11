module Lab6 where
import Data.List
import System.Random
import Test.QuickCheck
import Numeric
import Data.Char

import Lecture6

-- Exercise 1 - time spent: 2 hours
-- based on https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation

-- decToBin function used from: https://stackoverflow.com/a/1959734
decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a


-- Convert power y to base 2 and put in reversed list.
-- Loop over list, if 1 than use output else just calculate power for next function
exM' :: Integer -> Integer -> Integer -> Integer
exM' x y n = fex x n (reverse (decToBin y)) x

fex :: Integer -> Integer ->  [Integer] -> Integer -> Integer
fex x n [] t = 1
fex x n (k:ks) t | k == 1 = multM t (fex x n ks tn) n
                 | otherwise = fex x n ks tn
  where tn = multM t t n





-- Exercise 2 -- 10 minutes
-- With larger exponents there is a clear time saving as shown for the following calculations

-- *Lab6> exM' 234567 8910111 12131415 ==> 400068 | (0.00 secs, 108,072 bytes)
-- *Lab6> expM 234567 8910111 12131415 ==> 400068 | (1.42 secs, 59,654,784 bytes)

-- *Lab6> exM' 234567 89101111 12131415 ==> 288243 | (0.00 secs, 114,024 bytes)
-- *Lab6> expM 234567 89101111 12131415 ==> 288243 | (16.45 secs, 562,987,800 bytes)



-- Exercise 3
