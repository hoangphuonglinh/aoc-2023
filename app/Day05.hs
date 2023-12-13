module Day05 where
import Prelude hiding (split)
import qualified Data.Map as M
import Utils (split, printDay, splitLine, sdecimal)
import Data.List (minimumBy)
import Text.Gigaparsec
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Char
import Data.Maybe

data PlantNeeds = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location
   deriving (Show, Read, Eq)
type Seed = (PlantNeeds, Int)
type Range = (Int, Int, Int)
type Converter = (PlantNeeds, PlantNeeds)
type RMapEntry = (Converter, [Range])
type RMap = [RMapEntry]
type Bound = (Int, Int)
type Seeds = (PlantNeeds, [Bound])

process2 :: Seeds -> [Bound] -> RMap -> Int
process2 (Location, ranges) _ _ = minimum (map fst ranges)
process2 (pn, rs) nextr cmap
    | null rs = process2 (dest, nextr) [] cmap
    | otherwise = process2 (pn, bs++rs') (newr:nextr) cmap
        where
            ((l, u):rs') = rs
            ((_, dest), ranges) = head (filter (\((n, _), _) -> n == pn) cmap)
            (inrange, nomap@(ml, mu):bs) = breakRange
            range = filter (\(d, s, r) -> l >= s && l < s+r) ranges
            (d, s, r) = head range
            mapped = (d-s+ml, d-s+mu) 
            newr = if inrange then mapped else nomap

            breakRange :: (Bool, [Bound])
            breakRange 
                | null range && null lhigher = (False, [(l,u)]) 
                | null range = if nexthr > u then (False, [(l,u)]) else (False, [(l, nexthr-1), (nexthr, u)]) 
                | upper >= u = (True, [(l, u)]) 
                | otherwise = (True, [(l, upper), (upper+1, u)])
                    where 
                        upper = s+r-1
                        lhigher = filter (\(_, s', _) -> s' >= l) ranges
                        nexthr = minimum (map (\(_, s', _) -> s') lhigher)

process :: Seed -> RMap -> Int
process (Location, i) _ = i
process (pn, i) cmap = process (dest, val) cmap
   where
        ((_, dest), ranges) = head (filter (\((n, _), _) -> n == pn) cmap)
        val = findVal i ranges

findVal :: Int -> [Range] -> Int
findVal int [] = int
findVal int ((dv, sv, r):rs) 
    | int >= sv && int < sv+r = dv+(int-sv)
    | otherwise = findVal int rs

parsePlantNeeds :: Parsec PlantNeeds
parsePlantNeeds = choice
    [atomic $ Seed <$ string "seed",
     atomic $ Soil <$ string "soil",
     Fertilizer <$ string "fertilizer",
     Water <$ string "water",
     atomic $ Light <$ string "light",
     Temperature <$ string "temperature",
     Humidity <$ string "humidity",
     atomic $ Location <$ string "location"
    ]

parseRange :: Parsec Range
parseRange = do
    ps <- exactly 3 sdecimal
    return (head ps, ps !! 1, ps !! 2)

parseMap :: Parsec RMapEntry
parseMap = do
    t1 <- parsePlantNeeds
    string "-to-"
    t2 <- parsePlantNeeds
    whitespaces *> string "map:" *> whitespaces
    ranges <- some (parseRange <* endOfLine)
    return ((t1, t2), ranges)

parseAll :: Parsec ([Int], RMap)
parseAll = do
    seeds <- string "seeds:" *> whitespace *> some sdecimal
    whitespaces
    nms <- some (parseMap <* whitespaces)
    return (seeds, nms)

joinRange :: [Int] -> [Bound]
joinRange [] = []
joinRange (x:y:xys) = (x, x+y-1):joinRange xys

solve :: IO ()
solve = do
    inp <- readFile "inputs/Day05.inp"
    let (Success res) = parse @String parseAll inp
    let seeds = fst res
    let cmap = snd res
    let ans = minimum (map (curry (`process` cmap) Seed) seeds)

    let seedranges = joinRange seeds
    let ans2 = process2 (Seed, seedranges) [] cmap

    printDay 5 ans ans2

