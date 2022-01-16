module Day18.Part1 (solve) where

import Data.List (foldl1')
import Day18.Common (magnitude, parse, plus)

solve :: [String] -> Integer
solve = toInteger . magnitude . foldl1' plus . map parse
