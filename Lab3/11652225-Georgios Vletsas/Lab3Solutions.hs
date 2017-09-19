module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


--Task 1-------------------------------------
--satisfiable :: Form -> Bool
--satisfiable f = any (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\ v -> not(evl v f)) (allVals f)

tautology :: Form -> Bool
tautology f = all (\v -> (evl v f)) (allVals f)

-- logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)


contrTest = Cnj[q,Neg q]
tautTest = Dsj[q, Neg q]
entailOne = Cnj[Impl p q,Impl q r]
entailTwo = Impl p r
equivOne = Dsj[p,q]
equivTwo = Dsj[q,p]

taskOneTest = do
     putStrLn "Contradiction Test:"
     print (contradiction contrTest)
     putStrLn "Tautology Test:"
     print (tautology tautTest)
     putStrLn "Logical Entailment Test:"
     print (entails entailOne entailTwo)
     putStrLn "Logical Equivalence Test:"
     print (equiv equivOne equivTwo)
--1hr 20 min

--Task 2--------------------------------------

testParse :: Form -> Bool
testParse f = parse (show f) == [f]

parsingTest = all testParse [form1,form2,form3,Dsj[form1,form2],Cnj[form2,form3],Impl form1 form2]

--30 min

--Task 3--------------------------------------
--http://www.cse.unsw.edu.au/~meyden/teaching/cs2411/lectures/lecture7.pdf

cnftest = Cnj [Impl p q, Impl q r]


cnfgen :: Form -> Form
cnfgen  = conjForm . nnf . arrowfree 

conjForm :: Form -> Form
conjForm f@(Prop x) = f
conjForm f@(Neg (Prop x)) = f 
conjForm (Cnj fs) = Cnj (map conjForm fs)
conjForm (Dsj []) = Dsj []
conjForm (Dsj [f]) = conjForm f
conjForm (Dsj (f:fs)) = deMorgan (conjForm f) (conjForm (Dsj fs))

deMorgan :: Form -> Form -> Form
deMorgan (Cnj []) _     = Cnj []
deMorgan (Cnj [f]) g    = deMorgan f g
deMorgan (Cnj (f:fs)) g = Cnj [deMorgan f g, deMorgan (Cnj fs) g]
deMorgan _ (Cnj [])     = Cnj []
deMorgan f (Cnj [g])    = deMorgan f g
deMorgan f (Cnj (g:gs)) = Cnj[deMorgan f g, deMorgan f (Cnj gs)]
deMorgan f g            = Dsj [f,g]

testcnf = cnfgen cnftest


--3 hours 30 minutes


--Task4----------------------------------------------

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomForm :: Int -> IO Form
getRandomForm 0 = do x <- getRandomInt 10
                     return (Prop (x+1))
 
getRandomForm d = do x <- getRandomInt 10
                     case d of
                       0 -> return (Prop (x+1))
                       1 -> do f <- getRandomForm (0)
                               return (Neg f)
                       2 -> do f <- genRandomCD x
                               return (Cnj f)
                       3 -> do f <- genRandomCD x
                               return (Dsj f)

genRandomCD :: Int -> IO [Form]
genRandomCD n = do x <- getRandomInt 10
                   let g = Prop (x+1)
                   let gs = Prop (x-n)
                   return [g,gs]



main :: IO Form
main = do d <- getRandomInt 3
          getRandomForm d



--So far 2 hours 30 mins





