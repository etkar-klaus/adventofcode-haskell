module Day22.Part1 (solve) where

import Day22.Common (Cube (..), combine, intersects, parseAll, totalVolume)

solve :: [String] -> Integer
solve = totalVolume . combine . limitArea . parseAll

limitArea :: [Cube] -> [Cube]
limitArea = filter $ intersects $ Cube True (-50) 50 (-50) 50 (-50) 50
