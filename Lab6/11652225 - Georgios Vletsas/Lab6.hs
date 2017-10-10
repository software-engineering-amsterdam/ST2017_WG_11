module Lab6 where
import Data.List
import System.Random
import Test.QuickCheck
import Numeric
import Data.Char

import Lecture6

--Task 1 - time spent:
{-

-}

{-Task 3 -- time spent 10 mins
composites :: [Integer]
composites = filter (not.prime) [4..]

Found in line 148 of Lecture6.hs -}

--Task 7 (Bonus) --Time spent: 2 hours
--https://en.wikipedia.org/wiki/RSA_(cryptosystem)
{-This program generates random prime numbers which are checked via the Miller-Rabin primality check
Then primepair creates a pair of such primes, our (p,q)
Then it goes through the process of finding the public and private key through the
RSA Algorithm.
Where:
    n = p * q
    λ(n) = lcm (p-1)(q-1)
    e is coprime to λ(n) and smaller than
    d = multiplicative inverse of e and λ(n)
Public key (n,e) and Private Key (d,n)
We then create a cipher of a number, and check that when dechiphered it is the same as the original.
-}
genPrimes :: Int -> IO Integer
genPrimes k = do
  p <- getStdRandom (randomR (2^(k-1), 2^k - 1))
  isPrime <- primeMR 10 p
  if isPrime then return p else genPrimes k


primePair :: Int -> IO (Integer, Integer)
primePair k = do
              p <- genPrimes k
              q <- genPrimes k
              if p /= q then return (p,q) else primePair k

keyGen :: Int -> IO ((Integer,Integer),(Integer,Integer))
keyGen k = do
             (p,q) <- primePair k
             let n = p*q
             let l = lcm (p-1) (q-1)
             e <- genPrimes (k-1)
             let d = invM e l
             print ("Public Key: " ++ show (n,e) ++ ". Private key: " ++ show (n,d))
             return ((n,e),(n,d))

--m is the number to cipher, k is the bitlength of the cipher
rsaAlg :: Integer -> Int -> IO Bool
rsaAlg m k = do
            ((n,e),(g,d)) <- keyGen k
            let c = rsaEncode (e,n) m
            let dc = rsaDecode (d,n) c
            print ("Initial No: " ++ show m ++ ". Cipher: " ++ show c ++ ". Decypher: " ++ show dc)
            return (m == dc)















--
