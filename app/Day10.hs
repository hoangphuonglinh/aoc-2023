module Day10 where
import Utils (printDay, split, splitLine, removePreSpace, replaceChar)
import Prelude hiding (split)
import Data.List ((\\), groupBy, sort)
import qualified Data.Map as M

type Pos = (Int, Int)
type Row = Int
type Col = Int
type Pipes = String

process :: M.Map Row Pipes -> Pos -> Pos -> Int -> [Pos] -> ([Pos], Int)
process pmap cur@(cr, cc) prev step allpos
   | curpipe == 'S' = (allpos, step)
   | otherwise = process pmap nextPipe cur (step+1) (cur:allpos)
      where 
         curpipe = getPipe pmap cur
         nextPipe :: Pos
         nextPipe
            | curpipe == '|' = head (filter (/=prev) [up, down])
            | curpipe == 'L' = head (filter (/=prev) [up, right])
            | curpipe == '-' = head (filter (/=prev) [left, right])
            | curpipe == 'J' = head (filter (/=prev) [up, left])
            | curpipe == '7' = head (filter (/=prev) [left, down])
            | curpipe == 'F' = head (filter (/=prev) [right, down])
               where 
                  [up, down, left, right] = [(cr-1, cc), (cr+1, cc), (cr, cc-1), (cr, cc+1)]

findFirst :: M.Map Row Pipes -> Pos -> Pos
findFirst pmap (r', c')
   | up == '7' || up == '|' || up == 'F' = u
   | dp == 'L' || dp == '|' || dp == 'J' = d
   | lp == 'L' || lp == '-' || lp == 'F' = l
   | otherwise = r
      where 
         pos@[u, d, l, r] = [(r'+1, c'), (r'-1, c'), (r', c'-1), (r', c'+1)]
         [up, dp, lp, rp] = map (getPipe pmap) pos

getPipe :: M.Map Row Pipes -> Pos -> Char
getPipe pmap (r, c) = (pmap M.! r) !! c

-- if meet | L J not in pipe loop then flip isIn, if meet char not in pipe loop and isIn then count+1
process2 :: M.Map Row Pipes -> (Row, [Col]) -> Int
process2 pmap (r, cs) = go 0 False 0
   where
      go :: Int -> Bool -> Int -> Int
      go ccol isIn count 
         | ccol == last cs = count
         | curpipe `elem` switch && ccol `elem` cs = go (ccol+1) (not isIn) count 
         | ccol `notElem` cs && isIn = go (ccol+1) isIn (count+1) 
         | otherwise = go (ccol+1) isIn count
            where
               curpipe = getPipe pmap (r, ccol)
               switch = "|LJ"

solve :: IO ()
solve = do
   inp <- readFile "inputs/Day10.inp"
   let lines = splitLine inp
   let linewrow = zip [0..] lines
   let rows = head [(i, r) | (i, r) <- linewrow, 'S' `elem` r]
   let startrow = fst rows
   let startcol = head [i | (i, c) <- zip [0..] (snd rows), c == 'S']
   let startpos = (startrow, startcol)
   let pipemap = M.fromList linewrow
   let firstmove = findFirst pipemap startpos
   let (loopcord, steps) = process pipemap firstmove startpos 0 [startpos]
   let ans = (steps+1) `div` 2

   let loopmap =  [(head r, c) | rcs <- groupBy (\(r1, c1) (r2, c2) -> r1 == r2) (sort loopcord), 
                                 let (r, c) = unzip rcs]
   let inp' = replaceChar inp (startrow*length (head lines) +startcol) 'L'
   let pipemap2 = M.fromList (zip [0..] (splitLine inp'))
   let ans2 = sum (map (process2 pipemap2) loopmap)
   printDay 10 ans ans2

