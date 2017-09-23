module Lab3 where
import Data.List
import Data.Char
import Lecture3

-- Question 1
-- Time spent: (1hour)
-- Implementation
-- Description of your method of checking the definitions

-- Contradiction -> there should be no true evaluation, all evaluations should yield false
contradiction :: Form -> Bool
contradiction f = all (\x -> not $ evl x f) (allVals f)
-- form containing a contradiction
form4 = Neg form3

-- Tautology -> all evaluations should yield true
tautology :: Form -> Bool
tautology f = all (\x -> evl x f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 =  tautology (Impl f1 f2)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- TEST TODO

-- Question 2
-- Time spent: 1h

testParse :: String -> Bool
testParse f1 = show (head $ parse f1) == f1

-- Question 3
-- Time spent:

convertToCNF :: Form -> Form

-- (1) Convert Impl to (not A v B)
-- (2) Push negations down to Atoms (possibly: convert double negations to atoms)
-- (3) Use distribution to convert to CNF
