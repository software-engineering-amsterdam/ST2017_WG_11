--Setup

module Lab1Answers where
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

--Workshop exercise 1 as a test (Working) took about 2 hours

f1, f2 :: Int -> Int
f1 = \ n -> sum [0..n]
f2 = \ n -> (n*(n+1)) `div` 2

test1 = quickCheckResult (\n -> n>=0 --> f1 n == f2 n)

--2 hrs spent, notes added in notes file

--Workshop exercise 2 (Task 1)

f21, f22  :: Int -> Int
f21 = \ n -> sum(map (^2) [1..n])
f22 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

test2 = quickCheckResult (\n -> n>=0 --> f21 n == f22 n)

--15 minutes spent. Notes added in notes file.

--Workshop exercise 3 (Task 1)

f31, f32 :: Int -> Int
f31 = \ n -> sum(map (^3) [1..n])
f32 = \ n -> (n*(n+1) `div` 2)^2

test3 = quickCheckResult (\n -> n>=0 --> f31 n == f32 n)

--5 minutes spent.

--Workshop exercise 4 (Task 2)



