module Day01.Common (calculate) where

calculate :: Int -> [String] -> Integer
calculate n input = toInteger $ length $ filter (> 0) (diff n $ map read input)

diff :: Int -> [Int] -> [Int]
diff n list = zipWith (-) (drop n list) list
