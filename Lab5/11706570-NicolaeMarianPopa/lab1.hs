module Lab5 where
 
import Data.List
import System.Random
import Lecture5a

eExample :: Grid
eExample = [[0,0,0, 3,0,0, 0,0,0],
            [0,0,0, 7,0,0, 3,0,0],
            [2,0,0, 0,0,0, 0,0,8],

            [0,0,6, 0,0,5, 0,0,0],
            [0,9,1, 6,0,0, 0,0,0],
            [3,0,0, 0,7,1, 2,0,0],

            [0,0,0, 0,0,0, 0,3,1],
            [0,8,0, 0,4,0, 0,0,0],
            [0,0,2, 0,0,0, 0,0,0]]

mExample :: Grid
mExample = [[9,0,6, 0,7,0, 4,0,3],
            [0,0,0, 4,0,0, 2,0,0],
            [0,7,0, 0,2,3, 0,1,0],

            [5,0,0, 0,0,0, 1,0,0],
            [0,4,0, 2,0,8, 0,6,0],
            [0,0,3, 0,0,0, 0,0,5],

            [0,3,0, 7,0,0, 0,5,0],
            [0,0,7, 0,0,5, 0,0,0],
            [4,0,5, 0,1,0, 7,0,8]]


type Position = (Row,Column)
type Constrnt = [[Position]]

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
   ys = filter (elem (r,c)) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)