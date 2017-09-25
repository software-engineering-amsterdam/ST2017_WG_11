module Lab3 where
import Data.List
import Data.Char
import Lecture3
import System.Random

-- Question 1
-- Time spent: 1h
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
-- Time spent: 4.5h

-- (1) Convert Impl to (not A v B)
-- (2) Push negations down to Atoms (possibly: convert double negations to atoms)
-- (3) Use distribution to convert to CNF

convertToCNF :: Form -> Form
convertToCNF f = applyIterate(nnf(arrowfree(f)))

applyIterate :: Form -> Form
applyIterate (Prop a) = Prop a
applyIterate (Neg a) = Neg a
applyIterate (Cnj list) = Cnj (map applyIterate list)
applyIterate (Dsj []) = Dsj []
applyIterate (Dsj [x]) = applyIterate x -- Disjuction of one property is the property itself
applyIterate (Dsj (x:xs)) = applyDistribution (applyIterate x) (applyIterate (Dsj xs))

a = Prop 1
b = Prop 2
c = Prop 3

formX = Dsj [Neg (a), Neg (b)]
formX' = Dsj [a, b]
formY = a

-- Apply Distribution rules
applyDistribution :: Form -> Form -> Form
applyDistribution (Cnj [f1]) f2 = applyDistribution f1 f2
applyDistribution (Cnj (x:xs)) f2 = Cnj [applyDistribution x f2, applyDistribution (Cnj xs) f2]
applyDistribution f1 (Cnj [f2]) = applyDistribution f1 f2
applyDistribution f1 (Cnj (x:xs)) = Cnj [applyDistribution f1 x, applyDistribution f1 (Cnj xs)]
applyDistribution f1 f2 = Dsj [f1,f2] -- a,b -> prop already in CNF

-- form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
-- form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
-- form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

form5 = Equiv (Neg (Cnj [p,q])) (Dsj [(Neg p),(Neg q)])
form6 = Dsj[Cnj[p,q],Cnj[p,(Neg q)]]
test = Neg (Dsj [p,q])

-- Question 4

-- formGen :: Form

-- randomInt :: Int -> IO [Int]
-- randomInt = getStdRandom random

newRand = randomIO :: IO Int
