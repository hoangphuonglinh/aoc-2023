{-# LANGUAGE TupleSections #-}
module Day11 where
import Utils (printDay, split, splitLine, removePreSpace)
import Prelude hiding (split)
import Data.List (transpose, tails)
import Data.Bifunctor (second)

type Pos = (Int, Int)

process :: (Pos, Pos) -> [Int] -> [Int] -> Int -> Int
process ((x1, y1), (x2, y2)) emptyr emptyc i = (xu-xl) + (yu-yl) + (er + ec)*(i-1)
        where
            (xl, xu) = if x1 > x2 then (x2, x1) else (x1, x2)
            (yl, yu) = if y1 > y2 then (y2, y1) else (y1, y2)
            er = length (filter (\r -> r >= xl && r <= xu) emptyr)
            ec = length (filter (\c -> c >= yl && c <= yu) emptyc)

findGalaxy :: (Int, String) -> [Pos]
findGalaxy (r, cs) = map (r,) gs
    where 
        indexcs = zip [0..] cs
        gs = map fst (filter (\(c, g) -> g == '#') indexcs)

solve :: IO ()
solve = do
    inp <- readFile "inputs/Day11.inp"
    let lines = splitLine inp
    let rows = zip [0..] lines
    let emptyRows = [i | (i, r) <- rows, all (== '.') r]
    let emptyCols = [i | (i, c) <- zip [0..] (transpose lines), all (== '.') c]
    let galaxy = concatMap findGalaxy rows
    let galpairs = [(x, y) | (x:ys) <- tails galaxy, y <- ys]

    let ans = sum (map (\gp -> process gp emptyRows emptyCols 2) galpairs)
    let ans2 = sum (map (\gp -> process gp emptyRows emptyCols 1000000) galpairs)
    printDay 11 ans ans2