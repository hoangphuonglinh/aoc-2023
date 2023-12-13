module Day03 where
import Prelude hiding (split)
import Data.List ((\\), minimumBy)
import Utils (split, splitLine, printDay)
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

type Coordinate = (Int, Int)
type Position = (Char, Coordinate)
type Numbers = (Int, [Coordinate])

process :: [Numbers] -> [Coordinate] -> [Int]
process [] _ = []
process (n:ns) symbols
   | anyAdj = num : process ns symbols
   | otherwise = process ns symbols
      where 
         (num, cords) = n
         row = fst (head cords)
         cordAdj = filter (\s -> fst s <= row + 1 && fst s >= row - 1) symbols
         anyAdj = any adj cordAdj

         adj :: Coordinate -> Bool
         adj (_, col) = or [col <= c+1 && col >= c-1 | (_, c) <- cords]

zipCord :: String -> Int -> [Position]
zipCord str ycord = zip str (zip (repeat ycord) [0..length str])

findSymbols :: [Position] -> [Coordinate]
findSymbols str = [i | (c, i) <- str, c /= '.' && not (isDigit c)]

findStars :: [Position] -> [Coordinate]
findStars str = [i | (c, i) <- str, c == '*']

findNumbers :: [Position] -> [(Int, [Coordinate])]
findNumbers [] = []
findNumbers str@(s:ss)
   | isDigit (fst s) = (numint, cords) : findNumbers rest
   | otherwise = findNumbers ss
      where
         (num, rest) = span (isDigit . fst) str
         (nums, cords) = unzip num
         numint = read nums :: Int

process2 :: [Numbers] -> [Coordinate] -> [Int]
process2 ns [] = []
process2 ns (s:ss)
   | length adj == 2 = (head adj * adj !! 1) : process2 ns ss
   | otherwise = process2 ns ss
      where
         adj = process ns [s]

solve :: IO ()
solve = do
   inp <- readFile "inputs/Day03.inp"
   let line = splitLine inp
   let cords = [zipCord l y | (l, y) <- zip line [0..]]
   let symbols = concatMap findSymbols cords
   let numbers = concatMap findNumbers cords
   let ans = sum (process numbers symbols) 
   let stars = concatMap findStars cords
   let ans2 = sum (process2 numbers stars)
   printDay 3 ans ans2