module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--Exercise 3
--a)

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

--Defining the properties to test
p1,p2,p3, p4 :: Int -> Bool
p1 n = even n && n > 3
p2 n = even n || n > 3
p3 n = (even n && n>3) || even n
p4 n = even n

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


data FunctionParts a b = FunctionParts {name :: String , prop :: (a->Bool)}

myP1 = FunctionParts " even n AND n > 3 " p1
myP2 = FunctionParts " even n OR n > 3 " p2
myP3 = FunctionParts " (even n AND n>3) OR even n " p3
myP4 = FunctionParts " even " p4

sortConditions x y | (stronger set (prop x) (prop y)) = LT
                   | (weaker set (prop x) (prop y))  = GT
                   | otherwise = EQ
                   where set = [-10..10]

sortLst = [ name n | n<-sortBy sortConditions [myP1 , myP2 , myP3 , myP4 ]]

--Time spent: 1hr 10 min


--Task 4

--a)
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] xs = False
isPermutation xs [] = False
isPermutation xs (y:ys) | length xs /= length (y:ys) = False
                        | otherwise = isPermutation (delete y xs) ys


--Time Spent=40 mins
--b)

test1, test2 :: Eq a => [a] -> Bool
test1 x = isPermutation (reverse x) x --same list reversed
test2 x = isPermutation x x --same list

test3 :: Ord a => [a] -> Bool
test3 x = isPermutation x (sort x)

t4x = [1,2,3]
t4y = [4,5,2]
test4 :: Eq a => [a] -> [a] -> Bool
test4 x y = isPermutation x y

testWithSet :: Int -> ([Int] -> Bool) -> Bool
testWithSet n testNo = testNo [0..n]
--not containing duplicates means that lists of length [0..n] are good enough to
--prove the correctness.

--Time Spent: 30 mins

--Task 5

--a)
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] (x:xs) = False
isDerangement (x:xs) [] = False
isDerangement (x:xs) (y:ys) | length (x:xs) /= length (y:ys) = False
                            | x /= y = isDerangement (delete x xs) (delete y ys)
                            | otherwise = False





--Time Spent: 30 min (tried to make it work with isPermutation)

--Task6
{- ROT13 is a Caesar cipher that substitutes the first 13 letters of the latin
alphabet with the leter that is 13 letters after it. It can be inversed the same way
It needs to use the English alphabet(26 letters).

Implementation : read character. If it's up to the 13th, do +13. If it is from 13-26 then do -13

-}
rot13 :: Char -> Char
rot13 c = if c `elem` "ABCDEFGHIJKLM" || c `elem` "abcdefghijklm"
          then toEnum (fromEnum c + 13)
		  else if c `elem` "NOPQRSTUVWXYZ" || c `elem` "nopqrstuvwxyz"
		  then toEnum (fromEnum c - 13)
		  else c

--Time spent: 1hr

--Task7

iban :: String -> Bool
iban xs = (read $ ibanCalculation xs) `mod` 97 == 1


swapFirstFour :: String -> String
swapFirstFour xs = filter (/=' ') ((drop 4 xs) ++ (take 4 xs))

convertElement :: Char -> [Char]
convertElement c | c `elem` ['0'..'9'] = [c]
           | toUpper c `elem` ['A'..'Z'] = show (ord (toUpper c) - 55)
           | otherwise = error "Invalid Input"

ibanNumbers :: String -> [[Char]]
ibanNumbers xs = map convertElement $ swapFirstFour xs

ibanCalculation :: String -> String
ibanCalculation xs = concat $ ibanNumbers xs
--concat turns ["a","b","c"] into "abc"

--Valid ibanNumbers taken from http://www.rbs.co.uk/corporate/international/g0/guide-to-international-business/regulatory-information/iban/iban-example.ashx
testibanNumbers = ["AL47 2121 1009 0000 0002 3569 8741",
                   "AZ21 NABZ 0000 0000 1370 1000 1944",
                   "CY17 0020 0128 0000 0012 0052 7600",
                   "GR16 0110 1250 0000 0001 2300 695",
                   "IS14 0159 2600 7654 5510 7303 39",
                   "NL39 RABO 0300 0652 64",
                   "MT84 MALT 0110 0001 2345 MTLC AST0 01S",
                   "MU17 BOMM 0101 1010 3030 0200 000M UR"]

--Random fake iban Numbers. Note that the last one is from the valid list, but is invalid itself
--this was also checked using https://www.ibancalculator.com/iban_validieren.html
fakeibanNumbers = ["FAKE 4321 TEST 0923 8288",
                   "-S2Q EJS= WOAJ 2131 1231",
                   "NL39 RABO 2910 2993 52",
                   "GB29 RBOS 6016 1331 9268 19"]

testLegit = all iban testibanNumbers
--this tests all the iban numbers in testibanNumbers with the iban function. If all are true then it returns true

testFake =  all iban fakeibanNumbers
--this tests all the fake iban numbers in fakeibanNumbers with the iban function. If one of them is false, it returns False


--Time spent: 2hr 30min
