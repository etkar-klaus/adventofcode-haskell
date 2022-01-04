module Puzzles (Day (..), Part (..), solversForDay) where

import qualified Day01.Part1
import qualified Day01.Part2
import qualified Day02.Part1
import qualified Day02.Part2
import qualified Day03.Part1
import qualified Day03.Part2

data Day = Day01 | Day02 | Day03 | Day04
  deriving (Eq, Show, Enum, Bounded)

data Part = Part1 | Part2
  deriving (Eq, Show, Enum, Bounded)

solversForDay :: Day -> ([String] -> Integer, [String] -> Integer)
solversForDay Day01 = (Day01.Part1.solve, Day01.Part2.solve)
solversForDay Day02 = (Day02.Part1.solve, Day02.Part2.solve)
solversForDay Day03 = (Day03.Part1.solve, Day03.Part2.solve)
solversForDay Day04 = error "Not yet implemented"
