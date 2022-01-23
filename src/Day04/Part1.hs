module Day04.Part1 (solve) where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Day04.Common (finished, valueOf)

solve :: [String] -> Integer
solve = valueOf $ fromJust . find finished
