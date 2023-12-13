module Day07 where
import Prelude hiding (split)
import Utils (split, printDay, replace, splitLine, sdecimal)
import Data.List (nub, sortBy)
import qualified Data.Map as M
import Text.Gigaparsec
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Char

data HandType = HighCard | OnePair | TwoPairs | ThreeOAK | FullHouse | FourOAK | FiveOAK
   deriving (Show, Read, Eq, Ord)
data CardVal = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
   deriving (Show, Read, Eq, Ord)
type Cards = [CardVal]
type Hand = (Cards, Int)
type HandAndType = (HandType, Hand)

getType :: Hand -> Bool -> HandAndType
getType cb@(c, b) j = (findType c j, cb)

findType :: Cards -> Bool -> HandType 
findType c j
   | uniquel == 5 = HighCard
   | uniquel == 4 = OnePair
   | uniquel == 3 = if null cards3 then TwoPairs else ThreeOAK
   | uniquel == 2 = if null cards4 then FullHouse else FourOAK
   | uniquel == 1 = FiveOAK
      where
         unique = nub c
         uniquel' = length unique
         uniquel
            | j || (jcount' == 0) = uniquel'
            | otherwise = max 1 (uniquel' - 1)
         jcount' = length (filter (== Joker) c)
         jcount = if j then 0 else jcount'
         cards3 = filter (==3) (map (\s -> length (filter (==s) c) +jcount) unique)
         cards4 = filter (==4) (map (\s -> length (filter (==s) c) +jcount) unique)

sortHands :: HandAndType -> HandAndType -> Ordering  
sortHands (t1, (c1, _)) (t2, (c2, _))
   | t1 == t2 = compare c1 c2
   | otherwise = compare t1 t2
 
parseCard :: Parsec CardVal
parseCard = choice 
   [Two <$ string "2",
    Three <$ string "3",
    Four <$ string "4",
    Five <$ string "5",
    Six <$ string "6",
    Seven <$ string "7",
    Eight <$ string "8",
    Nine <$ string "9",
    Ten <$ string "T",
    Queen <$ string "Q",
    King <$ string "K",
    Ace <$ string "A"
   ]

parsep1 :: Parsec [CardVal]
parsep1 = do
   many (parseCard <|> (Jack <$ string "J"))

parsep2 :: Parsec [CardVal]
parsep2 = do
   many (parseCard <|> (Joker <$ string "J"))

parseHand :: Parsec [CardVal] -> Parsec Hand
parseHand parsecard = do
   cards <- parsecard <* whitespaces
   bid <- sdecimal <* whitespaces
   return (cards, bid)

solve :: IO ()
solve = do
   inp <- readFile "inputs/Day07.inp"
   let (Success res) = parse @String (many (parseHand parsep1)) inp
   let handtypes1 = map (`getType` True) res
   let sorthands1 = sortBy sortHands handtypes1
   let ans = sum (zipWith (\(_, (_, b)) i -> b*i) sorthands1 [1..])

   let (Success res2) = parse @String (many (parseHand parsep2)) inp
   let handtypes2 = map (`getType` False) res2
   let sorthands2 = sortBy sortHands handtypes2
   let ans2 = sum (zipWith (\(_, (_, b)) i -> b*i) sorthands2 [1..])
   printDay 7 ans ans2