module Day02.Part2 (solve) where

import Day02.Common (Command (..), parse)

solve :: [String] -> Integer
solve input = prod $ foldl exec (0, 0, 0) $ parse input

exec :: (Int, Int, Int) -> Command -> (Int, Int, Int)
exec (pos, depth, aim) (Forward value) = (pos + value, depth + aim * value, aim)
exec (pos, depth, aim) (Down value) = (pos, depth, aim + value)
exec (pos, depth, aim) (Up value) = (pos, depth, aim - value)

prod :: (Int, Int, Int) -> Integer
prod (pos, depth, _) = toInteger pos * toInteger depth
