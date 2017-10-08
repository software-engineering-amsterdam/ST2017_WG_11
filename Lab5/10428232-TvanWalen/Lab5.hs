module Lab5 where
import Data.List
import System.Random
import Test.QuickCheck

import Lecture5


-- Exercise 1 - 2,5 hours

nRCsolveAndShow :: Grid -> IO[()]
nRCsolveAndShow gr = nRCsolveShowNs (nRCinitNode gr)

nRCinitNode :: Grid -> [Node]
nRCinitNode gr = let s = grid2sud gr in
              if (not . nRCconsistent) s then []
              else [(s, nRCconstraints s)]

nRCblocks :: [[Int]]
nRCblocks = [[2..4],[6..8]]

nRCbl :: Int -> [Int]
nRCbl x = concat $ filter (elem x) nRCblocks

nRCsubGrid :: Sudoku -> (Row,Column) -> [Value]
nRCsubGrid s (r,c) =
 [ s (r',c') | r' <- nRCbl r, c' <- nRCbl c ]

nRCsubgridInjective :: Sudoku -> (Row,Column) -> Bool
nRCsubgridInjective s (r,c) = injective vs where
 vs = filter (/= 0) (nRCsubGrid s (r,c))


nRCconsistent :: Sudoku -> Bool
nRCconsistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ nRCsubgridInjective s (r,c) |
                    r <- [2,6], c <- [2, 6]]

nRCconstraints :: Sudoku -> [Constraint]
nRCconstraints s = sortBy length3rd
    [(r,c, nRCfreeAtPos s (r,c)) |
          (r,c) <- openPositions s ]

nRCfreeAtPos :: Sudoku -> (Row,Column) -> [Value]
nRCfreeAtPos s (r,c) =
 (freeInRow s r)
  `intersect` (freeInColumn s c)
  `intersect` (freeInSubgrid s (r,c))
  `intersect` (nRCfreeInSubgrid s (r,c))


nRCfreeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
nRCfreeInSubgrid s (r,c) = freeInSeq (nRCsubGrid s (r,c))



nRCsolveShowNs :: [Node] -> IO[()]
nRCsolveShowNs = sequence . fmap showNode . nRCsolveNs

nRCsolveNs :: [Node] -> [Node]
nRCsolveNs = nRCsearch nRCsuccNode solved

nRCsuccNode :: Node -> [Node]
nRCsuccNode (s,[]) = []
nRCsuccNode (s,p:ps) = nRCextendNode (s,ps) p

nRCextendNode :: Node -> Constraint -> [Node]
nRCextendNode (s,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         nRCprune (r,c,v) constraints) | v <- vs ]

nRCprune :: (Row,Column,Value)
     -> [Constraint] -> [Constraint]
nRCprune _ [] = []
nRCprune (r,c,v) ((x,y,zs):rest)
 | r == x = (x,y,zs\\[v]) : nRCprune (r,c,v) rest
 | c == y = (x,y,zs\\[v]) : nRCprune (r,c,v) rest
 | sameblock (r,c) (x,y) =
       (x,y,zs\\[v]) : nRCprune (r,c,v) rest
 | nRCsameblock (r,c) (x,y) =
       (x,y,zs\\[v]) : nRCprune (r,c,v) rest
 | otherwise = (x,y,zs) : nRCprune (r,c,v) rest

nRCsameblock :: (Row,Column) -> (Row,Column) -> Bool
nRCsameblock (r,c) (x,y) = nRCbl r == nRCbl x && nRCbl c == nRCbl y

nRCsearch :: (node -> [node])
      -> (node -> Bool) -> [node] -> [node]
nRCsearch children goal [] = []
nRCsearch children goal (x:xs)
 | goal x    = x : nRCsearch children goal xs
 | otherwise = nRCsearch children goal ((children x) ++ xs)

nRCexample :: Grid
nRCexample = [[0,0,0, 3,0,0, 0,0,0],
             [0,0,0, 7,0,0, 3,0,0],
             [2,0,0, 0,0,0, 0,0,8],

             [0,0,6, 0,0,5, 0,0,0],
             [0,9,1, 6,0,0, 0,0,0],
             [3,0,0, 0,7,1, 2,0,0],

             [0,0,0, 0,0,0, 0,3,1],
             [0,8,0, 0,4,0, 0,0,0],
             [0,0,2, 0,0,0, 0,0,0]]




-- Exercise 2 - 1,5 hours
-- See Lab5-2.hs




-- Exercise 3 - 1 hour
-- If it returns a sudoku it is a minimum sudoku. We checked by rewriting the minimalize function to check all possibilites one more time.
main3 :: IO ()
main3 = do [r] <- rsolveNs [emptyN]
           showNode r
           s <- genProblem3 r
           showNode s

genProblem3 :: Node -> IO Node
genProblem3 n = do ys <- randomize xs
                   let m = minimalize n ys
                   let b = checkIsMinimized m
                   if b then
                     return m
                   else
                     return emptyN
                   where xs = filledPositions (fst n)

checkIsMinimized :: Node -> Bool
checkIsMinimized n = uniqueSol n && isMinimalized n xs
  where xs = filledPositions (fst n)

isMinimalized :: Node -> [(Row,Column)] -> Bool
isMinimalized n [] = True
isMinimalized n ((r,c):rcs) | uniqueSol n' = False
                            | otherwise    = isMinimalized n rcs
 where n' = eraseN n (r,c)



-- Exercise 4 - 3 hours

-- Based on the code of Manuel

subBlocks = [[(r,c) | r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks]

blockSubs :: Int -> [[[(Int,Int)]]]
blockSubs n = filter (\bl -> length bl == n) (subsequences subBlocks)

genProblem4 :: Int -> Node -> IO Node
genProblem4 b n = do
                  let m = removeSubGrids n (blockSubs b)
                  return m

-- Should recall main4 if problem wasn't found.
main4 :: IO ()
main4 = do [r] <- rsolveNs [emptyN]
           showNode r
           s <- genProblem4 3 r
           showNode s
           s2 <- genProblem4 4 r
           showNode s2

removeSubGrids :: Node -> [[[(Int,Int)]]] -> Node
removeSubGrids n [] = emptyN
removeSubGrids n (sg:sgs) | checkIsMinimized m = m
                          | otherwise = removeSubGrids n sgs
                          where m = minimalize rsg rsgfilled
                                rsg = removeSubs n (concat sg)
                                rsgfilled = filledPositions (fst rsg)

removeSubs :: Node -> [(Row,Column)] -> Node
removeSubs n [] = n
removeSubs n ((r,c):rcs) = removeSubs (eraseN n (r,c)) rcs


-- genProblem4 :: Int -> Node -> Node
-- genProblem4 b n = removeSubGrids n (blockSubs b)
--
-- isEmptyNode :: Node -> Bool
-- isEmptyNode n = filledPositions n == []
--
-- main4 :: IO ()
-- main4 = do [r] <- rsolveNs [emptyN]
--            let s2 = genProblem4 4 r
--            if (isEmptyNode s2)
--              then
--                return main4
--              else
--                showNode r
--                showNode s2
--                s <- genProblem4 3 r
--                showNode s





-- Exercise 5 - 15 minutes (mostly based on the first solution, there I was also first looking to generate ranodm NRC problems)
nRCrsuccNode :: Node -> IO [Node]
nRCrsuccNode (s,cs) = do xs <- getRandomCnstr cs
                         if null xs
                           then return []
                           else return
                             (nRCextendNode (s,cs\\xs) (head xs))

nRCrsolveNs :: [Node] -> IO [Node]
nRCrsolveNs ns = rsearch nRCrsuccNode solved (return ns)

main5 :: IO ()
main5 = do [r] <- nRCrsolveNs [emptyN]
           showNode r
           s  <- genProblem r
           showNode s
