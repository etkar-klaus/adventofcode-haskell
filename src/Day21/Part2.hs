module Day21.Part2 (solve) where

import Data.Map (fromList, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

data Wins = Wins Integer Integer

data Player = Player {- position :: -} Int {- score :: -} Int
  deriving (Eq, Ord)

data State = State Player Player
  deriving (Eq, Ord)

maxScore :: Int
maxScore = 21

solve :: [String] -> Integer
solve = result . move . initState . map parse

parse :: String -> Int
parse = read . drop 28

initState :: [Int] -> State
initState [pos1, pos2] = State (Player pos1 0) (Player pos2 0)
initState _ = error "Wrong number of elements"

roll :: State -> Int -> Wins
roll (State (Player pos score) player2) die =
  let newPos = (pos + die - 1) `mod` 10 + 1
      newScore = score + newPos
   in if newScore >= maxScore
        then Wins 1 0
        else swap $ memoizedMove $ State player2 (Player newPos newScore)

move :: State -> Wins
move state =
  let f = mul . roll state
   in foldl add (Wins 0 0) [f 3 1, f 4 3, f 5 6, f 6 7, f 7 6, f 8 3, f 9 1]

memoizedMove :: State -> Wins
memoizedMove state =
  fromMaybe (error "not found") $
    lookup state $
      fromList
        [ let s = State (Player p1 s1) (Player p2 s2) in (s, move s)
          | p1 <- [1 .. 10],
            p2 <- [1 .. 10],
            s1 <- [0 .. maxScore],
            s2 <- [0 .. maxScore]
        ]

swap :: Wins -> Wins
swap (Wins win1 win2) = Wins win2 win1

add :: Wins -> Wins -> Wins
add (Wins win11 win12) (Wins win21 win22) = Wins (win11 + win21) (win12 + win22)

mul :: Wins -> Integer -> Wins
mul (Wins win1 win2) weight = Wins (win1 * weight) (win2 * weight)

result :: Wins -> Integer
result (Wins win1 win2) = max win1 win2
