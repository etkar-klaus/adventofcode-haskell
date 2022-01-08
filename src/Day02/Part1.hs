module Day02.Part1 (solve) where

import Data.Foldable (Foldable (foldl'))
import Data.Function (on)
import Day02.Common (Command (..), parse)

solve :: [String] -> Integer
solve input = uncurry ((*) `on` toInteger) $ foldl' exec (0, 0) $ parse input

exec :: (Int, Int) -> Command -> (Int, Int)
exec (pos, depth) (Forward value) = (pos + value, depth)
exec (pos, depth) (Down value) = (pos, depth + value)
exec (pos, depth) (Up value) = (pos, depth - value)
