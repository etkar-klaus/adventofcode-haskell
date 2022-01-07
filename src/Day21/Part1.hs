module Day21.Part1 (solve) where

data Player = Player {- position :: -} Int {- score :: -} Int

data State
  = State
      {- dice :: -} [Int]
      {- rolls :: -} Int
      {- player1 :: -} Player
      {- player2 :: -} Player

maxScore :: Int
maxScore = 1000

solve :: [String] -> Integer
solve = result . until end next . initState . map parse

parse :: String -> Int
parse = read . drop 28

initState :: [Int] -> State
initState [pos1, pos2] = State (cycle [1 .. 100]) 0 (Player pos1 0) (Player pos2 0)
initState _ = error "Wrong number of elements"

end :: State -> Bool
end (State _ _ _ (Player _ score2)) = score2 >= maxScore

next :: State -> State
next (State dice rolls pl1 pl2) = State (drop 3 dice) (rolls + 3) pl2 (pl1 `move` take 3 dice)

move :: Player -> [Int] -> Player
move (Player pos score) dice =
  let newPos = (pos + sum dice - 1) `mod` 10 + 1
   in Player newPos (score + newPos)

result :: State -> Integer
result (State _ rolls (Player _ score1) _) = toInteger score1 * toInteger rolls
