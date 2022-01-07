module Puzzles (Day (..), Part (..), solversForDay) where

import qualified Day01.Part1
import qualified Day01.Part2
import qualified Day02.Part1
import qualified Day02.Part2
import qualified Day03.Part1
import qualified Day03.Part2
import qualified Day21.Part1
import qualified Day21.Part2

data Day
  = Day01
  | Day02
  | Day03
  | Day04
  | Day05
  | Day06
  | Day07
  | Day08
  | Day09
  | Day10
  | Day11
  | Day12
  | Day13
  | Day14
  | Day15
  | Day16
  | Day17
  | Day18
  | Day19
  | Day20
  | Day21
  | Day22
  | Day23
  | Day24
  | Day25
  deriving (Eq, Show, Enum, Bounded)

data Part = Part1 | Part2
  deriving (Eq, Show, Enum, Bounded)

solversForDay :: Day -> ([String] -> Integer, [String] -> Integer)
solversForDay Day01 = (Day01.Part1.solve, Day01.Part2.solve)
solversForDay Day02 = (Day02.Part1.solve, Day02.Part2.solve)
solversForDay Day03 = (Day03.Part1.solve, Day03.Part2.solve)
solversForDay Day21 = (Day21.Part1.solve, Day21.Part2.solve)
solversForDay _ = error "Not yet implemented"
