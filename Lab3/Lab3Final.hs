module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

--Todo: add time spent
--comments
--structure code (task4 mainly)

--Task 1-------------------------------------
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

-- We know that a tautology is also satisviable
-- We know that a contradiction can't be satisfiable and thus not a tautology
--

fContra1 = Cnj [p, Neg p]
fSatis1 = Cnj [p, Neg q]
fSatis2 = Impl (Dsj [p, q]) p
fTauto1 = Dsj [p, Neg p]
fTauto2 = Impl p (Dsj [p, q])

-- return True if all testcases are valid
test1 = foldl (&&) True
  [contradiction fContra1,
   not (contradiction fSatis1),
   not (contradiction fSatis2),
   not (contradiction fTauto1),
   not (contradiction fTauto2),
   tautology fTauto1,
   tautology fTauto2,
   not (tautology fSatis1),
   not (tautology fSatis2),
   not (tautology fContra1),
   entails form1 form3,
   entails form2 form1,
   entails form2 form3,
   not (entails form1 form2),
   not (entails form3 form2),
   equiv form1 form3,
   not (equiv form1 form2),
   not (equiv form2 form3)]

taskOneTest = do
     putStrLn "Contradiction Test:"
     print (contradiction contrTest)
     putStrLn "Tautology Test:"
     print (tautology tautTest)
     putStrLn "Logical Entailment Test:"
     print (entails entailOne entailTwo)
     putStrLn "Logical Equivalence Test:"
     print (equiv equivOne equivTwo)
     putStrLn "Test all properties with multiple forms:"
     print (test1)
	 
	 
--task 2


testParse :: Form -> Bool
testParse f = parse (show f) == [f]

parsingTest = all testParse [form1,form2,form3,Dsj[form1,form2],Cnj[form2,form3],Impl form1 form2]

testF1 = Impl p (Dsj [p, q]) -- tests testE4 expression
testS1 = "(1==>+(1 2))"
testF2 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r) --tests form3 expression
testS2 = "(*((1==>2) (2==>3))==>(1==>3))"
test1stCase = all (==True) [(show testF1) == testS1, (show testF2) == testS2]

--task 3

form2cnf :: Form -> Form
form2cnf = db . nnf . arrowfree

f1 = Dsj [p, Neg q]
f12 = Dsj [p, Cnj [q, Neg r]]
f2 = Cnj [p, Neg p]

db :: Form -> Form
db (Prop x)         = Prop x
db (Neg (Prop x))   = Neg (Prop x)
db (Dsj [f])        = db f
db (Dsj (f:fs))     = deMorgan (db f) (db (Dsj fs))
db (Cnj fs)         = Cnj (map db fs)

deMorgan :: Form -> Form -> Form
deMorgan (Cnj [a]) b    = Dsj [a, b]
deMorgan a (Cnj [b])    = Dsj [a, b]
deMorgan (Cnj (a:as)) b = Cnj [Dsj [a, b], deMorgan (Cnj as) b]
deMorgan a (Cnj (b:bs)) = Cnj [Dsj [a, b], deMorgan a (Cnj bs)]
deMorgan a b            = Dsj [a, b]


--Task 4

-- Random number generations from: https://stackoverflow.com/questions/22526629/am-i-using-randomrio-wrong
getRandomInt :: Int -> IO Int
getRandomInt n = randomRIO (1,n) :: IO Int

genRandomForm :: IO Form
genRandomForm = do
  r <- getRandomInt 2 :: IO Int
  l <- getRandomInt 3 :: IO Int
  f <- getRandomForm r l :: IO Form
  return f

newRandomForm :: Int -> IO Form
newRandomForm s = do
  r1 <- getRandomInt 4
  f1 <- getRandomForm r1 (s-1)
  return f1

newRandomForms :: Int -> IO [Form]
newRandomForms s = do
  f1 <- newRandomForm (s)
  f2 <- newRandomForm (s)
  return [f1, f2]

getRandomForm :: Int -> Int -> IO Form
getRandomForm _ 0 = do
                      r <- getRandomInt 10
                      return (Prop r)
getRandomForm 0 s = do
                      f <- newRandomForm s
                      return (Neg (f))
getRandomForm 1 s = do
                      f <- newRandomForms s
                      return (Cnj f)
getRandomForm 2 s = do
                      f <- newRandomForms s
                      return (Dsj f)
getRandomForm 3 s = do
                      f <- newRandomForms s
                      let f1 = head f
                      let f2 = head (tail f)
                      return (Impl f1 f2)
getRandomForm 4 s = do
                      f <- newRandomForms s
                      let f1 = head f
                      let f2 = head (tail f)
                      return (Equiv f1 f2)


-- to test if the generated cnf is valid we have to test if it's still equivalent to the original formula.
loop :: Int -> IO ()
loop 0 = do return ()
loop n = do
  f <- genRandomForm
  let t = equiv f (form2cnf f)
  if t then putStrLn ("It works for: "++ show f)
    else putStrLn ("It doens't work for: "++ show f)
  loop (n-1)