module Utils where
import Data.Char
import Data.Maybe
import qualified Data.Array as A
import Text.Gigaparsec
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Char

split :: String -> Char -> [String]
split "" _ = [""]
split (x:xs) c 
    | x == c && n /= "" = "":next
    | x == c = next
    | otherwise = (x:n) : ns
        where
            next@(n:ns) = split xs c

splitLine :: String -> [String]
splitLine str = split str '\n'

tabulate :: A.Ix i => (i, i) -> (i -> a) -> A.Array i a
tabulate (u, v) f = A.array (u, v) [(i, f i) | i <- A.range (u, v)]

stringToInts :: [String] -> [Int]
stringToInts str = [read x :: Int | x <- str]

replace :: [Int] -> Int -> Int -> [Int]
replace [] _ _ = []
replace (x:xs) ind num
    | ind /= 0 = x: replace xs (ind-1) num
    | otherwise = num:xs

--not counting \n
replaceChar :: [Char] -> Int -> Char -> [Char]
replaceChar [] _ _ = []
replaceChar (x:xs) ind num
    | x == '\n' = x: replaceChar xs ind num
    | ind /= 0 = x: replaceChar xs (ind-1) num
    | otherwise = num:xs

removeAll :: String -> Char -> String
removeAll [] _ = []
removeAll (x:xs) c
    | x == c = removeAll xs c
    | otherwise = x : removeAll xs c

removeSpace :: String -> String
removeSpace xs = removeAll xs ' '

removePreSpace :: String -> String
removePreSpace [] = []
removePreSpace str@(s:ss)
    | isSpace s = removePreSpace ss
    | otherwise = str



sdecimal :: Parsec Int
sdecimal = do
    s <- option $ char '-'
    cs <- some digit
    spaces
    return (read (fromMaybe ' ' s:cs))

printDay :: (Show a, Show b) => Int -> a -> b -> IO ()
printDay num p1 p2 = do 
    putStrLn ("----------------- DAY " ++ show num ++ "-----------------")
    putStr "PART 1: "
    print p1
    putStr "PART 2: "
    print p2
