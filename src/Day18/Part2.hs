module Day18.Part2 (solve) where

import Day18.Common (magnitude, parse, plus)

solve :: [String] -> Integer
solve = toInteger . maximum . map (magnitude . uncurry plus) . pairs . map parse

pairs :: [a] -> [(a, a)]
pairs (x : xs) = zip xs (repeat x) ++ zip (repeat x) xs ++ pairs xs
pairs _ = []
