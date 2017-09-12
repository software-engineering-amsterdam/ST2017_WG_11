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

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

--Ex1. time spent: 30min
testProbs :: Int -> IO [Int]
testProbs 0 = return [0,0,0,0]
testProbs n = do
              list <- probs n
              let q1 = length (filter (\x -> x > 0 && x < 0.25) list)
              let q2 = length (filter (\x -> x >= 0.25 && x < 0.5) list)
              let q3 = length (filter (\x -> x >= 0.5 && x < 0.75) list)
              let q4 = length (filter (\x -> x >= 0.75 && x < 1) list)
              return [q1,q2,q3,q4]

--Ex2. time spent: 45 mins
data Shape = NoTriangle | Equilateral
           | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | (a > b) || (a > c) || (b > c) = error ("Please give lengths in ascending order")
               | a == b && b  == c = Equilateral
               | (a^2) + (b^2) < c^2 = NoTriangle
               | (a == b) || (a == c) || (b == c) = Isosceles
               | (a^2) + (b^2) == c^2 = Rectangular
               | otherwise = Other

--Ex3. time spent: 150 mins
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

data FunctionPair a b = FunctionPair {
    name :: String
    , prop :: (a->Bool)
}

p1, p2, p3, p4 :: Int -> Bool
p1 x = even x && x > 3
p2 x = even x || x > 3
p3 x = (even x && x > 3) || even x
p4 x = even x

myP1 = FunctionPair "p1" p1
myP2 = FunctionPair "p2" p2
myP3 = FunctionPair "p3" p3
myP4 = FunctionPair "p4" p4

--sortCond :: FunctionPair -> FunctionPair -> Ordering
sortCond a b | (stronger set (prop a) (prop b)) = LT
             | (weaker set (prop a) (prop b)) = GT 
             | otherwise = EQ
             where set = [-10..10]

test = [name x | x<-sortBy sortCond [myP1, myP2, myP3, myP4]]

--Ex4. time spent: 45 mins
quicksrt :: Ord a => [a] -> [a]  
quicksrt [] = []  
quicksrt (x:xs) = 
   quicksrt [ a | a <- xs, a <= x ]  
   ++ [x]
   ++ quicksrt [ a | a <- xs, a > x ]

isPermutationSrt, isDerangement :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutationSrt a b | a == b = True
                     | otherwise = (quicksrt a) == (quicksrt b)

sameLengthProp :: [a] -> [a] -> Bool
sameLengthProp a b = length a == length b

index :: (Eq a) => a -> [a] -> Int
index a (x:xs) | a == x = 1
               | otherwise = 1 + index a xs

testMap :: (Eq a) => [a] -> [a] -> Bool
testMap xs ys = foldl (&&) True (map (\(x,y) -> index x xs /= index x ys) (zip xs ys))

--Ex5 - 30 min
isDerangement xs ys = (isPermutationSrt xs ys) && (foldl (&&) True (map (\(x,y) -> x /= y) (zip xs ys)))
--isDerangement xs ys = (isPermutationSrt xs ys) && (foldl (&&) True (map \x -> index x xs /= index x ys) xs))

deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x set) (permutations set) where set = [0..n-1]