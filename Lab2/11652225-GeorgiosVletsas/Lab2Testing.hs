module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


  
  
loop :: Int -> IO ()
loop n = do
    if n <= 10
	then do
	   putStrLn (show ( n * n ))
       loop ( n + 1 )
	else
       return ()

main :: IO ()
main = loop 1


loop :: Int -> IO ()
loop n = do
    if n <= 10
    then do
        putStrLn (show (n * n))
        loop (n + 1)
    else
        return ()

main :: IO ()
main = loop 1

--Why does 16 to 16 not work and 29 to 39 works? no tabs on either