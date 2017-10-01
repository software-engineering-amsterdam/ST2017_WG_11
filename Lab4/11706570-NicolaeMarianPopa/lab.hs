module Lab4 where
import Data.List
import System.Random
import Test.QuickCheck

import SetOrd
import Lecture4
import Lecture2

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos a = sort $ a ++ [(y,x) | (x,y) <- a, not $ elem (y,x) a]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

a = [(1,2),(2,3),(3,4)]

--time spent: 30 mins
trClos :: Ord a => Rel a -> Rel a
trClos a | a == b = b
    | otherwise = (trClos b)
    where b = sort $ nub $ a ++ (a @@ a)

--checks whether all the first list contains all the elements from the second one
contains :: Eq a => [a] -> [a] -> Bool
contains a b = all (\x -> elem x a) b

--properties
isSymmetric :: Eq a => Rel a -> Bool
isSymmetric a = contains a rev
                where rev = [(y,x) | (x,y) <- a]

isSymmetric2 :: Ord a => Rel a -> Bool
isSymmetric2 [] = True
isSymmetric2 ((x,y):xs) | elem (y,x) xs = isSymmetric2 (delete (y,x) xs)
                       | otherwise = False

isTransitive :: Eq a => Rel a -> Bool
isTransitive a = contains a (a @@ a)

--all relations created by symClos are symmetric
symClosProp a = isSymmetric (symClos a)

--all relations created by trClos are transitive
trClosProp a = isTransitive (trClos a)

-- getRandomRel :: IO (Rel Int)
-- getRandomRel = do
--     l <- getRandomInt 20
--     x <- getIntL 10 l
--     y <- getIntL 10 l
--     return (zip x y)

testM :: Int -> Int -> (Rel a -> Rel a)
                    -> (Rel a -> Bool) -> IO ()
testM k n clos prop = if k == n then print (show n ++ " tests passed")
                else do
                    l <- getRandomInt 20
                    x <- getIntL 10 l
                    y <- getIntL 10 l
                    let xs = (zip x y) :: Rel Int
                    let ts = clos xs
                    if prop ts then
                      do print ("pass on: " ++ show xs)
                         testM (k+1) n clos prop
                    else error ("failed test on: " ++ show xs)

-- main :: Integer -> IO ([(Int, Int)])
-- main 0 = return []
-- main n = do
--     a <- getRandomInt 10
--     b <- getRandomInt 10
--     return ((a,b):(main (n-1)))