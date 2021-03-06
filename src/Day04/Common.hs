module Day04.Common (finished, valueOf, markRepeat) where

import Data.Array (Array, elems, listArray, (!), (//))
import Data.List (elemIndex)
import Data.List.Split (chunksOf, splitOn)

data Bingo = Bingo {_marks :: Array Int Bool, _numbers :: [Int], _lastNum :: Int}
  deriving (Show)

valueOf :: ([Bingo] -> Bingo) -> [String] -> Integer
valueOf select = toInteger . value . select . play

play :: [String] -> [Bingo]
play input = concat $ markRepeat (parseBingos input) (parseNumbers $ head input)

bingo :: [[Int]] -> Bingo
bingo nums = Bingo (listArray (0, 24) $ repeat False) (concat nums) (-1)

parseBingos :: [String] -> [Bingo]
parseBingos = map (parseBingo . tail) . chunksOf 6 . tail

parseBingo :: [String] -> Bingo
parseBingo = bingo . map parseCells

parseCells :: String -> [Int]
parseCells = map read . words

parseNumbers :: String -> [Int]
parseNumbers = map read . splitOn [',']

markRepeat :: [Bingo] -> [Int] -> [[Bingo]]
markRepeat = scanl $ flip (map . markMaybe) . filter (not . finished)

markMaybe :: Int -> Bingo -> Bingo
markMaybe num (Bingo marks numbers _) = maybe Bingo setMark (elemIndex num numbers) marks numbers num

setMark :: Int -> Array Int Bool -> [Int] -> Int -> Bingo
setMark index marks = Bingo (marks // [(index, True)])

finished :: Bingo -> Bool
-- finished b = any (horizontal b) [0 .. 4] || any (vertical b) [0 .. 4]
finished b = or $ [horizontal b, vertical b] <*> [0 .. 4]

horizontal :: Bingo -> Int -> Bool
horizontal (Bingo marks _ _) row = all (marks !) [row * 5 + col | col <- [0 .. 4]]

vertical :: Bingo -> Int -> Bool
vertical (Bingo marks _ _) col = all (marks !) [row * 5 + col | row <- [0 .. 4]]

value :: Bingo -> Int
value (Bingo marks numbers lastNum) = condSum (elems marks) numbers * lastNum

condSum :: [Bool] -> [Int] -> Int
condSum marks nums = sum $ zipWith (\m n -> if m then 0 else n) marks nums
