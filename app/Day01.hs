module Day01 where
import Prelude hiding (split)
import Data.Char
import qualified Data.Map as M
import Utils (split, printDay, splitLine, stringToInts)

digitsmap = zip ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ['0'..]

process :: [String] -> Int
process str = sum (stringToInts nums)
    where
        numl = map (filter isDigit) str
        nums = zipWith (\a b -> a:[b]) (map head numl) (map last numl)

isPrefix :: String -> String -> Bool -> (Bool, Char)
isPrefix str sub b = ((length str >= ls) && (take ls str == sub), M.findWithDefault ' ' val (M.fromList digitsmap))
    where 
        ls = length sub
        val = if b then sub else reverse sub

findNum :: String -> Bool -> Char
findNum str@(s:ss) b
    | isDigit s = s
    | not (null bcs) = snd (head bcs)
    | otherwise = findNum ss b
        where 
            dm' = map fst digitsmap
            dm = if b then dm' else map reverse dm'
            bcs' = map (\d -> isPrefix str d b) dm
            bcs = filter fst bcs'

process2 :: [String] -> Int
process2 str = sum (stringToInts (zipWith (\a b -> a:[b]) (map (`findNum` True) str) (map ((`findNum` False) . reverse) str)))

solve :: IO ()
solve = do
    inp <- readFile "inputs/Day01.inp"
    let line = split inp '\n'
    let ans = process line
    let ans2 = process2 line
    printDay 1 ans ans2