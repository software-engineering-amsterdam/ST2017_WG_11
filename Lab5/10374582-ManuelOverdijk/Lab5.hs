
module Lab5 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture5


-- Exercise 1
-- Time spent: 2h 15m

blocksNRC :: [[Int]]
blocksNRC = [[2..4], [6..8]]

blNRC :: Int -> [Int]
blNRC x = concat $ filter (elem x) blocksNRC

subGridNRC :: Sudoku -> (Row,Column) -> [Value]
subGridNRC s (r,c) =
  [ s (r',c') | r' <- blNRC r, c' <- blNRC c ]

solveAndShowNRC :: Grid -> IO[()]
solveAndShowNRC gr = solveShowNRC (initNodeNRC gr)

solveShowNRC :: [Node] -> IO[()]
solveShowNRC = sequence . fmap showNode . solveNsNRC

solveNsNRC :: [Node] -> [Node]
solveNsNRC = search succNodeNRC solved

consistentNRC :: Sudoku -> Bool
consistentNRC s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]
               ++
               [ subgridInjectiveNRC s (r,c) | r <- [2,6], c <- [2,6]]


initNodeNRC :: Grid -> [Node]
initNodeNRC gr = let s = grid2sud gr in
              if (not . consistentNRC) s then []
              else [(s, constraintsNRC s)]

constraintsNRC :: Sudoku -> [Constraint]
constraintsNRC s = sortBy length3rd
    [(r,c, freeAtPosNRC s (r,c)) | (r,c) <- openPositions s ]


freeAtPosNRC :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNRC s (r,c) =
  (freeInRow s r)
    `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInNRCgrid s (r,c))


pruneNRC :: (Row,Column,Value)
      -> [Constraint] -> [Constraint]
pruneNRC _ [] = []
pruneNRC (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | sameblock (r,c) (x,y) =
          (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | sameblockNRC (r,c) (x,y) =
        (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | otherwise = (x,y,zs) : pruneNRC (r,c,v) rest


sameblockNRC :: (Row,Column) -> (Row,Column) -> Bool
sameblockNRC (r,c) (x,y) = blNRC r == blNRC x && blNRC c == blNRC y

subgridInjectiveNRC :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNRC s (r,c) = injective vs where
   vs = filter (/= 0) (subGridNRC s (r,c))

freeInNRCgrid :: Sudoku -> (Row,Column) -> [Value]
freeInNRCgrid s (r,c) = freeInSeq (subGridNRC s (r,c))


succNodeNRC :: Node -> [Node]
succNodeNRC (s,[]) = []
succNodeNRC (s,p:ps) = extendNodeNRC (s,ps) p


extendNodeNRC :: Node -> Constraint -> [Node]
extendNodeNRC (s,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         pruneNRC (r,c,v) constraints) | v <- vs ]

exampleNRC :: Grid
exampleNRC = [[0,0,0, 3,0,0, 0,0,0],
             [0,0,0, 7,0,0, 3,0,0],
             [2,0,0, 0,0,0, 0,0,8],
             [0,0,6, 0,0,5, 0,0,0],
             [0,9,1, 6,0,0, 0,0,0],
             [3,0,0, 0,7,1, 2,0,0],
             [0,0,0, 0,0,0, 0,3,1],
             [0,8,0, 0,4,0, 0,0,0],
             [0,0,2, 0,0,0, 0,0,0]]


-- Exercise 3
-- Time spent: 2.5h

mainMinimal :: IO ()
mainMinimal = do [r] <- rsolveNs [emptyN]
                 s  <- genProblem r
                 let firstPosition = filledPositions (fst s)
                 let firstElement = firstPosition!!0
                 let node = eraseN s firstElement
                 print "Sudoku:"
                 showNode s
                 if uniqueSol node then print "is not unique, and not minimal" else print "is unique, and minimal"



