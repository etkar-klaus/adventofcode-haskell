module Day03.Part1 (solve) where

import GHC.Base (Applicative (liftA2))

solve :: [String] -> Integer
solve input = res $ foldl (zipWith (+)) (repeat 0) ((map . map) parseInt input)

parseInt :: Char -> Int
parseInt '0' = -1
parseInt '1' = 1
parseInt _ = error "Invalid argument"

res :: [Int] -> Integer
res = liftA2 (*) (toInt . map (> 0)) (toInt . map (< 0))

toInt :: [Bool] -> Integer
toInt = foldl (\r v -> r * 2 + (if v then 1 else 0)) 0
