module Lab4 where
import Data.List
import System.Random
import Test.QuickCheck

import SetOrd
import Lecture4


-- Exercise 2 -- 1.5 hours
-- First wanted to try to do it based on an empty set and add random numbers but that was harder than expected. This is a lot easier. We are wodering how this could be done without using lists?
getRandomList :: Int -> Int -> IO [Int]
getRandomList x 0 = return []
getRandomList x n = do
  r <- getRandomInt x
  l <- getRandomList x (n-1)
  return (r : l)

getRandomInt :: Int -> IO Int
getRandomInt n = randomRIO (1,n) :: IO Int

randomSet :: Int -> Int -> IO (Set Int)
randomSet x n = do
  r <- getRandomInt n
  l <- getRandomList x r
  let  s = list2set l
  return s

-- Exercise 5 -- 45 minutes
-- Just switch the pairs and then use the union function to filter out the repeating elements if they are in the list.
type Rel a = [(a,a)]
a = [(1,2),(2,3),(3,4)]
b = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

symSet :: Ord a => Rel a -> Rel a
symSet [] = []
symSet ((x,y):xs) = (y,x) : symSet xs

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos xs = sort (nub (union xs (symSet xs)))



-- Exercise 6 -- 45 minutes
-- In this solution we used a possible property of a transitive closure and we are wondering if this is correct.
-- Basically we assume that when no new transitive Relation can be found we have found all of them.
infixr 5 @@
c = [(1,2),(2,3),(3,4),(50,51)]
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trSet :: Ord a => Rel a -> Rel a
trSet [] = []
trSet xs | length u /= length xs = trSet (u)
         | otherwise = u
         where u = union (xs @@ xs) xs

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos xs = sort (nub (trSet xs))


-- Exercise 7 - 1.5 hours
-- From book pp .179
-- We create a proof for symclos and transclos by defining a function isSymmetric and isTransitive to test if a symclos or transclos of a randomly generated list of pairs returns true. It should always do this.
d = [(1,2),(2,1)]
isSymmetric :: Ord a => Rel a -> Bool
isSymmetric [] = True
isSymmetric ((x,y):xs) | x == y = isSymmetric (xs)
                       | elem (y,x) xs = isSymmetric (delete (y,x) xs)
                       | otherwise = False

isTransitive :: Ord a => Rel a -> Bool
isTransitive [] = True
isTransitive xs | length (union (xs @@ xs) xs) == length xs = True
                | otherwise = False


testSymClos :: IO ()
testSymClos = do
    l1 <- getRandomList 5 5
    l2 <- getRandomList 5 5
    let z = (zip l1 l2) :: Rel Int
    let ts = symClos z
    let b = isSymmetric ts :: Bool
    if b then putStrLn ("This is symmetric: "++ show ts)
      else putStrLn ("This is NOT symmetric: "++ show ts)


testTransClos :: IO ()
testTransClos = do
    l1 <- getRandomList 5 5
    l2 <- getRandomList 5 5
    let z = (zip l1 l2) :: Rel Int
    let ts = trClos z
    let b = isTransitive ts :: Bool
    if b then putStrLn ("This is transitive: "++ show ts)
      else putStrLn ("This is NOT transitive: "++ show ts)
