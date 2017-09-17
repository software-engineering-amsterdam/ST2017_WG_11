{--
Lab 2
Terry van Walen (11652225)
Manuel Overdijk (10374582)
Georgios Vletsas (10428232)
Nicolae Marian Popa (11706570)


Final Solution for Exercises 1 - 7

--}
module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1)
  return (p:ps)


-- Exercise 1

testProbs :: Int -> IO [Int]
testProbs 0 = return [0,0,0,0]
testProbs n = do
              list <- probs n
              let q1 = length (filter (\x -> x > 0 && x < 0.25) list)
              let q2 = length (filter (\x -> x >= 0.25 && x < 0.5) list)
              let q3 = length (filter (\x -> x >= 0.5 && x < 0.75) list)
              let q4 = length (filter (\x -> x >= 0.75 && x < 1) list)
              return [q1,q2,q3,q4]


-- Test result report for n == 10000:
-- [2498,2551,2417,2534]
-- [2490,2528,2490,2492]
-- [2473,2507,2543,2477]
-- [2545,2455,2497,2503]
-- The result show that the implementation is correct.

-- Exercise 2


data Shape = NoTriangle | Equilateral
  | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape

triangle a b c =
   if a == b && b == c                                      then Equilateral
   else if (^2) a + (^2) b == (^2) c                        then Rectangular
   else if (^2) b + (^2) c == (^2) a                        then Rectangular
   else if (^2) c + (^2) a == (^2) b                        then Rectangular
   else if a <= 0 || b <= 0 || c <= 0                       then NoTriangle
   else if (a + b) <= c || (a + c) <= b || (c + b) <= a     then NoTriangle
   else if (a == b) || (b == c) || (c == a)                 then Isosceles
   else Other


testFunction :: Shape -> [[Integer]]
testFunction shape = [[x,y,z] | x <- [1..5], y <- [1..5], z <- [1..5], triangle x y z == shape ]

--Returns all triads in the range 1..5 that have the shape given in the function.


-- Exercise 3


stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- Question 3a, properties of Workshop Exercise 3

a, b, c :: Int -> Bool
a x = even x && x > 3
b x = even x || x > 3
c x = (even x && x > 3) || even x

q3a, q3b, q3c, q3d :: Bool
q3a = stronger [(-10)..10] a even
q3b = stronger [(-10)..10] b even
q3c = stronger [(-10)..10] c even
q3d = stronger [(-10)..10] even c

-- Question 3b, descending list of properties

aFn = FunctionName "a" a
bFn = FunctionName "b" b
cFn = FunctionName "c" c
dFn = FunctionName "d" c

data FunctionName a b = FunctionName {
      fnName :: String,
      fn :: (a->Bool)
}

orderProperties p q | (stronger l a b) = GT
                    | (weaker l a b) = LT
                    | otherwise = EQ
                    where a = (fn p)
                          b = (fn q)
                          l = [(-10)..10]

myOrdering = [ (fnName x) | x <- reverse (sortBy orderProperties [aFn, bFn, cFn, dFn])]

-- myOrdering = ["a","c","d","b"]

-- Exercise 4

quicksrt :: Ord a => [a] -> [a]
quicksrt [] = []
quicksrt (x:xs) =
   quicksrt [ a | a <- xs, a <= x ]
   ++ [x]
   ++ quicksrt [ a | a <- xs, a > x ]

isPermutation, isDerangement :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutation a b | a == b = True
                     | otherwise = (quicksrt a) == (quicksrt b)

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove a xs = filter (/=a) xs

sameLengthProp :: [a] -> [a] -> Bool
sameLengthProp a b = length a == length b

propA :: Ord a => [a] -> [a] -> Bool
propA a b = isPermutation a b == isPermutation b a

propB :: Ord a => [a] -> [a] -> [a] -> Bool
propB a b c | (isPermutation a b) && (isPermutation b c) = (isPermutation a c) == True
            | otherwise = False

index :: (Eq a) => a -> [a] -> Int
index a (x:xs) | a == x = 1
               | otherwise = 1 + index a xs

testMap :: (Eq a) => [a] -> [a] -> Bool
testMap xs ys = foldl (&&) True (map (\(x,y) -> index x xs /= index x ys) (zip xs ys))

-- Testing TODO


--Exercise 5
isDerangement xs ys = (isPermutation xs ys) && (foldl (&&) True (map (\(x,y) -> x /= y) (zip xs ys)))

deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x set) (permutations set) where set = [0..n-1]

-- Tests for exercise 5
propDeranLength :: Ord a => [a] -> [a] -> Bool
propDeranLength a b = isDerangement a b --> length a == length b

propDeranPerm :: Ord a => [a] -> [a] -> Bool
propDeranPerm a b = isDerangement a b --> isPermutation a b

-- Only the empty list is a derangement of itself
propDeranRefl :: Ord a => [a] -> Bool
propDeranRefl a = isDerangement a a --> null a

propDeranSym :: Ord a => [a] -> [a] -> Bool
propDeranSym a b = isDerangement a b == isDerangement b a

propDeranTrans :: Ord a => [a] -> [a] -> [a] -> Bool
propDeranTrans a b c = ((isDerangement a b) && (isDerangement b c)) --> (isDerangement a c)


-- Exercise 6

{- ROT13 is a Caesar cipher that substitutes the first 13 letters of the latin
alphabet with the leter that is 13 letters after it. It can be inversed the same way
It needs to use the English alphabet(26 letters).

Implementation : read character. If it's up to the 13th, do +13. If it is from 13-26 then do -13
--}
rot13 :: Char -> Char
rot13 c = if (toUpper c) `elem` ['A'..'M']
      then toEnum (fromEnum c + 13)
      else if (toUpper c) `elem` ['N'..'Z']
      then toEnum (fromEnum c - 13)
      else c

cipherString :: String -> String
cipherString xs = map rot13 xs

-- QuickCheck testable properties

cipherIsInversable s = cipherString (cipherString s) == s
-- quickCheck(cipherIsInversable) -> +++ OK, passed 100 tests.




--prop_onlyAlphabet str =

-- prop inverse is the same
-- prop only works on the alpabet
-- does not work on non-alphabet

-- Exercise 7

ibans = [("AD",24),("AT",20),("BH",22),("BE",16),("BA",20),("BG",22),("HR",21),("CY",28),("GB",22),("AL",28),("AZ",28),("GR",27),("IS",26),("NL",18),("MT",31),("MU",30)]

moveToBeginning, replaceWithNumbers :: String -> String
moveToBeginning str = (drop 4 str) ++ (take 4 str)

replaceWithNumbers "" = ""
replaceWithNumbers (x:xs) | (x>='A' && x<='Z') = show ((ord x) - 55) ++ replaceWithNumbers xs
                          | otherwise = x : replaceWithNumbers xs

checkCountry :: String -> Bool
checkCountry c | list == [] = False
               | otherwise = True
               where list = filter (\(x,y)-> x == (take 2 c)) ibans


iban :: String -> Bool
iban nr = checkCountry code && (length code == countryCodeLength) && (read (replaceWithNumbers (moveToBeginning code)) :: Integer) `mod` 97 == 1
    where countryCodeLength = snd (head (filter (\(x,y)-> x == (take 2 code)) ibans))
          code = filter (/=' ') nr

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

