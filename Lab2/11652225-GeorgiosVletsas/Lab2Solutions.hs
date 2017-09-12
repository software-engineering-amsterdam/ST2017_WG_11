module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--Exercise 4
--a)

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

--Defining the properties to test
p1,p2,p3 :: Int -> Bool
p1 n = even n && n > 3 
p2 n = even n || n > 3
p3 n = (even n && n>3) || even n

--functions that test the properties
f1,f2,f3, f12, f22, f32 :: Int -> Bool
f1 n = stronger [-n..n] even (p1)
f2 n = stronger [-n..n] even (p2)
f3 n = stronger [-n..n] even (p3)
f12 n = weaker [-n..n] even (p1)
f22 n = weaker [-n..n] even (p2)
f32 n = weaker [-n..n] even (p3)

--comparison of the properties with clear output
compar :: Int -> (Int -> Bool) -> (Int -> Bool) -> String
compar n x1 x2 = 
         if strong && weak then "Equivalent"
            else if strong then "Stronger"
            else if weak then "Weaker"
            else "Incomparable"
         where strong = x1 n
               weak = x2 n

--Just text output for clearer understanding of results
testProperties = do
   putStrLn "For the range [-10..10] we will check which property is stronger."
   putChar '\n'
   putStrLn "[ even ] compared to [ even n && n>3 ] for this range."
   print (compar 10 f1 f12)
   putStrLn "[ even ] compared to [ even n || n > 3 ] for this range."
   print (compar 10 f2 f22)
   putStrLn "[ even ] compared to [ (even n && n>3) || even n ] for this range."
   print (compar 10 f3 f32)
   
--time so far: 1 hr

--b)