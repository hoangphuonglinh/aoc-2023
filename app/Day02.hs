module Day02 where
import Prelude hiding (split)
import Data.Char
import Utils (split, splitLine, printDay)

data Colour = R | G | B
   deriving (Eq, Read, Show)
type GameId = Int
type Cube = (Colour, Int)
type Game = (GameId, [[Cube]])

filterColour :: [[Cube]] -> ([Cube], [Cube], [Cube])
filterColour cbs = (f R, f G, f B)
   where f cl = concatMap (filter (\c -> fst c == cl)) cbs

process :: Game -> Int
process (id, cubes)
   | f r 12 && f g 13 && f b 14 = id
   | otherwise = 0
      where
         (r, g, b) = filterColour cubes
         f cl n = all (\c -> snd c <= n) cl

process2 :: Game -> Int
process2 (id, cubes) = f r * f g * f b
   where
      (r, g, b) = filterColour cubes
      f c = maximum (map snd c)

convertType :: String -> Cube
convertType str@(s:ss)
   | isSpace s = convertType ss
   | isDigit s = (read [toUpper letter] :: Colour, read count :: Int)
      where 
         (count, rest) = span isDigit str
         (_:letter:_) = rest

format :: String -> [[Cube]]
format str = map (map convertType) str'
   where 
      str' = map (`split` ',') (split str ';')

solve :: IO ()
solve = do
   inp <- readFile "inputs/Day02.inp"
   let line = splitLine inp
   let splitGameNum = map (`split` ':') line
   let formatted = map (\g -> (read (dropWhile (not . isDigit) (head g)) :: Int, format (g !! 1))) splitGameNum
   let ans = sum (map process formatted)
   let ans2 = sum (map process2 formatted)
   printDay 2 ans ans2