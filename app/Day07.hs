module Day07 where
import Prelude hiding (split)
import Utils (split, printDay, replace, splitLine)
import Data.List (nub, sortBy)
import qualified Data.Map as M

data HandType = HighCard | OnePair | TwoPairs | ThreeOAK | FullHouse | FourOAK | FiveOAK
   deriving (Show, Read, Eq, Ord, Enum)
data CardVal = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
   deriving (Show, Read, Eq, Ord)
type Cards = String
type Hand = (Cards, Int)
type HandAndType = (HandType, Hand)

cardMap :: M.Map Char CardVal
cardMap = M.fromList (zip ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'] 
              [Two, Three, Four, Five, Six, Seven, Eight, Nine, T, J, Q, K, A])
              
cardMapJ :: M.Map Char CardVal
cardMapJ = M.fromList (zip ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'] 
              [Two, Three, Four, Five, Six, Seven, Eight, Nine, T, Joker, Q, K, A])

getType :: Hand -> Bool -> HandAndType
getType cb@(c, b) j = (findType c j, cb)

findType :: String -> Bool -> HandType 
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
            | j || noj = uniquel'
            | otherwise = max 1 (uniquel' - 1)
         jcount' = length (filter (== 'J') c)
         noj = jcount' == 0
         jcount = if j then 0 else jcount'
         cards3 = filter (==3) (map (\s -> length (filter (==s) c) +jcount) unique)
         cards4 = filter (==4) (map (\s -> length (filter (==s) c) +jcount) unique)

sortHands :: M.Map Char CardVal -> HandAndType -> HandAndType -> Ordering  
sortHands m (t1, (c1, _)) (t2, (c2, _))
   | t1 < t2 = LT
   | t1 > t2 = GT
   | otherwise = sortCards m c1 c2

sortCards :: M.Map Char CardVal -> Cards -> Cards -> Ordering
sortCards _ [] [] = EQ
sortCards m (c:c1) (c':c2) 
   | m M.! c < m M.! c' = LT
   | m M.! c > m M.! c' = GT
   | otherwise = sortCards m c1 c2
 
solve :: IO ()
solve = do
   inp <- readFile "inputs/Day07.inp"
   let lines = splitLine inp
   let cardbid = map (`split` ' ') lines 
   let cbs = map (\cb -> (head cb, read (cb !! 1) :: Int )) cardbid
   let handtypes = map (`getType` True) cbs 
   let sorthands = sortBy (sortHands cardMap) handtypes
   let ans = sum (zipWith (\(_, (_, b)) i -> b*i) sorthands [1..])

   let handtypesj = map (`getType` False) cbs 
   let sorthandsj = sortBy (sortHands cardMapJ) handtypesj
   let ans2 = sum (zipWith (\(_, (_, b)) i -> b*i) sorthandsj [1..])
   printDay 7 ans ans2