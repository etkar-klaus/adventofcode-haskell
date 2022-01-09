{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day21.Part2 (solve) where

import qualified Data.Array.Comfort.Boxed as Arr (fromList, (!))
import qualified Data.Array.Comfort.Shape as Sh (ZeroBased (..))
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec (fromList, (!))
import Prelude hiding (lookup)

data Wins = Wins Integer Integer

data Player = Player {- position :: -} Int {- score :: -} Int
  deriving (Eq, Ord)

data State = State Player Player
  deriving (Eq, Ord)

maxScore :: Int
maxScore = 21

memoizedCountWins :: State -> Wins
memoizedCountWins = memoizedCountWinsArray -- choose from: memoizedCountWinsArray, memoizedCountWinsMap, memoizedCountWinsList, memoizedCountWinsVector

countWins :: State -> Wins
countWins = countWinsCombined -- choose from: countWinsNaive, countWinsCombined

solve :: [String] -> Integer
solve = result . memoizedCountWins . initState . map parse

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
        else swap $ memoizedCountWins $ State player2 (Player newPos newScore)

-- | Naive implementation for counting wins.
-- Create all 27 possible outcomes of rolling 3 dice with 3 sides each.
-- This is considerably slower than countWinsCombined.
countWinsNaive :: State -> Wins
countWinsNaive state =
  foldl' add (Wins 0 0) [roll state (a + b + c) | a <- [1 .. 3], b <- [1 .. 3], c <- [1 .. 3]]

-- | Improved implementation for counting wins.
-- Creates the 7 distinct sums that 3 dice with 3 sides each can create.
-- Win counts will be multiplied by the number of different ways the sum can be created.
countWinsCombined :: State -> Wins
countWinsCombined state =
  let f = mul . roll state
   in foldl' add (Wins 0 0) [f 3 1, f 4 3, f 5 6, f 6 7, f 7 6, f 8 3, f 9 1]

-- | Memoization using Data.Map
memoizedCountWinsMap :: State -> Wins
memoizedCountWinsMap state =
  fromMaybe (error "not found") $
    Map.lookup state $
      Map.fromList
        [ (s, countWins s)
          | p1 <- [1 .. 10],
            p2 <- [1 .. 10],
            s1 <- [0 .. maxScore - 1],
            s2 <- [0 .. maxScore - 1],
            let s = State (Player p1 s1) (Player p2 s2)
        ]

-- | Memoization using an ordinary list with index access.
memoizedCountWinsList :: State -> Wins
memoizedCountWinsList state =
  [ countWins $ State (Player p1 s1) (Player p2 s2)
    | p1 <- [1 .. 10],
      p2 <- [1 .. 10],
      s1 <- [0 .. maxScore - 1],
      s2 <- [0 .. maxScore - 1]
  ]
    !! index state

-- | Memoization using Data.Vector with index access.
memoizedCountWinsVector :: State -> Wins
memoizedCountWinsVector state =
  Vec.fromList
    [ countWins $ State (Player p1 s1) (Player p2 s2)
      | p1 <- [1 .. 10],
        p2 <- [1 .. 10],
        s1 <- [0 .. maxScore - 1],
        s2 <- [0 .. maxScore - 1]
    ]
    Vec.! index state

-- | Memoization using Data.Array.Comfort.Boxed with index access.
memoizedCountWinsArray :: State -> Wins
memoizedCountWinsArray state =
  Arr.fromList
    (Sh.ZeroBased (10 * 10 * maxScore * maxScore))
    [ countWins $ State (Player p1 s1) (Player p2 s2)
      | p1 <- [1 .. 10],
        p2 <- [1 .. 10],
        s1 <- [0 .. maxScore - 1],
        s2 <- [0 .. maxScore - 1]
    ]
    Arr.! index state

-- | Create an index for lookup in linear memoization data structures.
index :: State -> Int
index (State (Player p1 s1) (Player p2 s2)) = ((p1 * 10 + p2 - 11) * maxScore + s1) * maxScore + s2

swap :: Wins -> Wins
swap (Wins win1 win2) = Wins win2 win1

add :: Wins -> Wins -> Wins
add (Wins win11 win12) (Wins win21 win22) = Wins (win11 + win21) (win12 + win22)

mul :: Wins -> Integer -> Wins
mul (Wins win1 win2) fac = Wins (win1 * fac) (win2 * fac)

result :: Wins -> Integer
result (Wins win1 win2) = max win1 win2
