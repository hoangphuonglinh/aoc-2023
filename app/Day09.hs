module Day09 where
import Utils (printDay, split, splitLine, removePreSpace)
import Prelude hiding (split)

process :: [Int] -> Bool -> Int
process nums@(n:ns) b
   | all (== n) ns = n
   | b = last nums + process nums' b
   | otherwise = n - process nums' b
      where
         nums' = zipWith (-) ns nums

solve :: IO ()
solve = do
   inp <- readFile "inputs/Day09.inp"
   let lines = splitLine inp
   let numlists = map (`split` ' ') lines
   let numbers = map (map (\n -> read n :: Int)) numlists
   let ans = sum (map (`process` True) numbers)
   let ans2 = sum (map (`process` False) numbers)
   printDay 9 ans ans2