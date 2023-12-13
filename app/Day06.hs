module Day06 where
import Prelude hiding (split)
import Utils (split, printDay, splitLine, removePreSpace, removeSpace)
import Data.List (minimumBy)

type Time = Int
type Distance = Int
type Race = (Time, Distance)

process :: Race -> Int
process (t, d) = sum (map beatScore [0..t])
   where
      beatScore :: Time -> Int
      beatScore p = if (t-p)*p >= d then 1 else 0

solve :: IO ()
solve = do
   inp <- readFile "inputs/Day06.inp"
   let lines = splitLine inp
   let td = map (\l -> last (split l ':')) lines
   let [times, dists] = map (\t -> map (\s -> read s :: Int) (split (removePreSpace t) ' ')) td
   let ans = product (zipWith (curry process) times dists)
   let [bigtime, bigdist] = map (\t -> read (removeSpace t) :: Int) td
   let ans2 = process (bigtime, bigdist)
   printDay 6 ans ans2