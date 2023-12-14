module Day13 where
import Utils (printDay, split, splitLine, removePreSpace, diff, tabulate)
import Prelude hiding (split)
import Data.List.Split
import Data.List (intercalate, transpose, (\\))

process :: [String] -> Int -> Int
process xs d = sum (map (findSym xs) [1..length xs-1])*100 + sum (map (findSym txs) [1..length txs-1])
    where
        txs = transpose xs
        findSym :: [String] -> Int -> Int
        findSym ss r   
            | d == difs = r
            | otherwise = 0
            where
                (f, s) = splitAt r ss
                difs = sum (zipWith diff (reverse f) s)

solve :: IO ()
solve = do
    inp <- readFile "inputs/Day13.inp"
    let patterns = splitOn "\n\n" inp
    let patternrows = map splitLine patterns
    let ans = sum (map (`process` 0) patternrows)
    let ans2 = sum (map (`process` 1) patternrows)
    printDay 13 ans ans2