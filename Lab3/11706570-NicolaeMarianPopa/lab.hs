module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

--unlike satisfiable, a tautology requires that ALL possible assignments evaluate to TRUE
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

--the implication must be a tautology
entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

equiv :: Form -> Form -> Bool
equiv a b = (entails a b) && (entails b a)

--For checking the function we thought of taking the exercises from the workshop and devise individual
--tests for each of them. One of us wrote the expressions in a Form form, while another made use of the
--supplied parser. I used the parser.
--Time spent: 1h 30min
testE1 = "+(1 -2)"
testP1 = (not $ tautology expr) && (satisfiable expr) && (not $ contradiction expr) where expr = head $ parse testE1

testE2 = "*(1 -1)"
testP2 = (not $ tautology expr) && (not $ satisfiable expr) && (contradiction expr) where expr = head $ parse testE2

testE3 = "+(1 -1)"
testP3 = (tautology expr) && (satisfiable expr) && (not $ contradiction expr) where expr = head $ parse testE3

testE4 = "(1==>+(1 2))"
testP4 = (tautology expr) && (satisfiable expr) && (not $ contradiction expr) where expr = head $ parse testE4

testE5 = "(+(1 2)==>1)"
testP5 = (not $ tautology expr) && (satisfiable expr) && (not $ contradiction expr) where expr = head $ parse testE5

testE6 = "+(1 -1)"
testE7 = "+(2 -2)"
testP6 = entails (head $ parse testE6) (head $ parse testE7)
testP7 = equiv (head $ parse testE6) (head $ parse testE7)

expressionsCheck = foldl (&&) True [testP1, testP2, testP3, testP4, testP5, testP6, testP7]

--ex2 1h30min
--For testing, we used form1, form2 and form3
--1st case: have the same expression written in both String and in Form, see if parse gets the same expression.
--2nd case: parse the outcome of (show form), see if it reaches the same value.
--The difference between them is that the first one does not rely on the implementation of show.

--1st case
testF1 = Impl p (Dsj [p, q]) -- tests testE4 expression
testS1 = "(1==>+(1 2))"
testF2 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r) --tests form3 expression
testS2 = "(*((1==>2) (2==>3))==>(1==>3))"
test1stCase = all (==True) [(show testF1) == testS1, (show testF2) == testS2]

--ex3, time spent: 3h15min
form2cnf :: Form -> Form
form2cnf = iterateF . nnf . arrowfree

--apply distribution principle
--the idea is to recursively iterate through the expression and apply the distribution principle
--whenever we find a disjunction.
--iterateF iterates does the iterating
--applyD applies the distribution principle by receiving the two sides of the disjunction.
--We found more information on the CNF form here:
--http://www.cse.unsw.edu.au/~meyden/teaching/cs2411/lectures/lecture7.pdf
--To get the final result iterateF is just composed with nnf and arrowfree functions
testF (Dsj (a:as)) = True
testF _ = False

iterateF :: Form -> Form
iterateF (Neg x) = Neg x
iterateF (Prop x) = Prop x
iterateF (Cnj xs) = Cnj (map iterateF xs) -- iterate over all elements in the conjunction
iterateF (Dsj []) = (Dsj [])
iterateF (Dsj [x]) = iterateF x
iterateF (Dsj (x:xs)) = applyD (iterateF x) (iterateF (Dsj xs))

--receives first term of the disjunction and the second term and return the conjunction
applyD :: Form -> Form -> Form

applyD a (Cnj [b]) = applyD a b
applyD a (Cnj (b:bs)) = Cnj[applyD a b, applyD a (Cnj bs)]

applyD (Cnj [a]) b = applyD a b
applyD (Cnj (a:as)) b = Cnj[applyD a b, applyD (Cnj as) b]

applyD a b = Dsj [a,b]

--testExpression = Neg (Cnj [Neg p, (Dsj [q, Neg (Cnj[p,r])])])
testExpression = Dsj[Cnj[p,q],Cnj[p,(Neg q)]]

iterateList [] = []
iterateList (x:xs) = (succ x) : iterateList xs

--ex5, time spent: 2hxmin
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- getRandomVar :: IO Form
-- getRandomVar = do
--                  n <- getRandomInt 10
--                  return (Prop (n + 1))

getRandomExprN :: Int -> IO [Form]
getRandomExprN 0 = return []
getRandomExprN n = do
                     expr <- getRandomExpr 2

getRandomExpr :: Int -> IO Form
getRandomExpr d = do
                    y <- getRandomInt 1
                    case y of
                      0 -> do
                        a <- getRandomInt 5
                        return (Prop (a + 1))
                      1 -> do
                      	expr <- getRandomExpr (d-1)
                        return (Neg expr)
