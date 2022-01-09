module Day22.Part2 (solve) where

import Day22.Common (combine, parseAll, totalVolume)

solve :: [String] -> Integer
solve = totalVolume . combine . parseAll
