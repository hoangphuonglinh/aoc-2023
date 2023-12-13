module Day08 where
import Prelude hiding (split)
import Utils (split, printDay)
import Text.Gigaparsec
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Char
import Data.Maybe
import qualified Data.Map as M

type Location = String
type Directions = [Char]
type Destination = (Location, Location)
type MapEntry = (Location, Destination)
type MapL = [MapEntry]

process :: Location -> Int -> Directions -> M.Map Location Destination -> Bool -> Int
process loc steps dir maplr b
   | (b && loc == "ZZZ") || (not b && last loc == 'Z') = steps
   | otherwise = process next (steps+1) dir maplr b
      where
         lr = dir !! (steps `mod` length dir)
         next = if lr == 'L' then l else r
         (l, r) = maplr M.! loc

parseMap :: Parsec MapEntry
parseMap = do
      src <- exactly 3 upper
      string " = ("
      d1 <- exactly 3 upper
      char ',' *> whitespaces
      d2 <- exactly 3 upper
      char ')'
      return (src, (d1, d2))

parseAll :: Parsec (String, MapL)
parseAll = do
      directions <- some upper
      whitespaces
      maps <- some (parseMap <* whitespaces)
      return (directions, maps)

solve :: IO ()
solve = do
   inp <- readFile "inputs/Day08.inp"
   let (Success res) = parse @String parseAll inp
   let directions = fst res
   let listmap = snd res 
   let maps = M.fromList listmap
   let ans = process "AAA" 0 directions maps True
   let start = [s | (s, _) <- listmap, last s == 'A']
   let zs = map (\s -> process s 0 directions maps False) start
   let ans2 = foldl1 lcm zs
   printDay 8 ans ans2
