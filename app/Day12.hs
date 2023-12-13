module Day12 where
import Utils (printDay, split, splitLine, removePreSpace, sdecimal, tabulate)
import Prelude hiding (split)
import Text.Gigaparsec
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Char
import Data.List (intercalate)
import qualified Data.Array as A

type REntry = ([Spring], [Int])
data Spring = O | D | U
    deriving (Show, Read, Eq)

process :: REntry -> Int
process (sprs, is) = table A.! (0, 0)
    where
        ls = length sprs
        li = length is

        table :: A.Array (Int, Int) Int
        table = tabulate ((0,0), (ls, li)) (\(x, y) -> calc (drop x sprs, drop y is))

        calc :: REntry -> Int
        calc ([], is) = if null is then 1 else 0
        calc (D:_, []) = 0
        calc (O:ss, is) = table A.! (ls-length ss, li-length is)
        calc (spr@(U:ss), is) = table A.! (ls-length ss, li-length is) + calc (D:ss, is)
        calc (spr@(D:_), i:is)
            | length spr >= i && O `notElem` cspr && (null rest || head rest /= D) = table A.! (ls - length (drop 1 rest), li-length is)
            | otherwise = 0
                where 
                    (cspr, rest) = splitAt i spr
    
parseSpring :: Parsec Spring
parseSpring = choice
    [O <$ string ".",
     D <$ string "#",
     U <$ string "?"
    ]

parseEntry :: Parsec REntry
parseEntry = do
    springs <- some parseSpring
    whitespaces
    damaged <- sepBy sdecimal (string ",")
    whitespaces
    return (springs, damaged)

solve :: IO ()
solve = do
    inp <- readFile "inputs/Day12.inp"
    let (Success res) = parse @String (some parseEntry) inp
    let ans = sum (map process res)
    let res2 = map (\(s, i) -> (intercalate [U] (replicate 5 s), concat (replicate 5 i))) res
    let ans2 = sum (map process res2)
    printDay 12 ans ans2