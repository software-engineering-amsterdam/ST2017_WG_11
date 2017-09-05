module Lab1Answers where
import Data.List
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

power :: Int -> [Int] -> [Int]
power p [] = []
power p (x:xs) = (x^p) : (power p xs)

f21, f22 :: Int -> Int
f21 = \n -> sum (power 2 [0..n])
f22 = \n -> div (product [n, n+1,2*n + 1]) 6
test2 = quickCheckResult (\n -> n >= 0 --> f21 n == f22 n)

f31, f32 :: Int -> Int
f31 = \n -> sum (power 3 [0..n])
f32 = \n -> (div (product [n, n+1]) 2)^2
test3 = quickCheckResult (\n -> n >= 0 --> f31 n == f32 n)
