module Lab1Answers where
import Data.List
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

power2 :: [Int] -> [Int]
power2 [] = []
power2 (x:xs) = (x^2) : (power2 xs)

f21, f22 :: Int -> Int
f21 = \n -> sum (power2 [0..n])
f22 = \n -> div (product [n, n+1,2*n + 1]) 6

test2 = quickCheckResult (\n -> n >= 0 --> f21 n == f22 n)
