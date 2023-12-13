{-# LANGUAGE TupleSections #-}
module Day04 where
import Utils (printDay, split, splitLine, removePreSpace)
import Data.List (nub, (\\))
import Data.Char
import Prelude hiding (split)
import qualified Data.Map as M

type Game = ([String], [String])

process2 :: [(Int, Game)] -> M.Map Int Int -> Int -> Int
process2 [] _ total = total
process2 ((gid, (w, h)):gs) cards total = process2 gs newcards (total+numcard)
   where
      numcard = M.findWithDefault 0 gid cards +1
      newcards = M.unionWith (+) cards (M.fromList (map (, numcard) [gid + 1 .. gid + ws]))

      ws = length h - length (h \\ w) 

process :: Game -> Int
process (w, h) = if ws == 0 then 0 else 2^(ws-1)
   where
      ws = length h - length (h \\ w)

solve :: IO ()
solve = do
   inp <- readFile "inputs/Day04.inp"
   let line = splitLine inp
   let splitGameNum' = map (`split` ':') line
   let splitGameNum = map (!!1) splitGameNum'
   let game' = map (`split` '|') splitGameNum 
   let game'' = map (map removePreSpace) game'
   let game = [(split (head g) ' ', split (g!!1) ' ') | g <- game''] 
   let ans = sum (map process game)
   let ans2 =  process2 (zip [1..] game) M.empty 0
   printDay 4 ans ans2