module Day03.Part2 (solve) where

import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (maximumBy, minimumBy, partition)
import Data.Ord (comparing)
import GHC.Base (Applicative (liftA2))

data Bit = B0 | B1 deriving (Eq, Show, Enum)

solve :: [String] -> Integer
solve = liftA2 (*) (rating True) (rating False) . map (decorate . parse)

rating :: Bool -> [([Bit], [Bit])] -> Integer
rating = (bitsToInt .) . findRow

bitsToInt :: [Bit] -> Integer
bitsToInt = toInteger . foldl' (\r v -> r * 2 + fromEnum v) 0

parse :: String -> [Bit]
parse = map $ toEnum . fromEnum . (== '1')

decorate :: a -> (a, a)
decorate xs = (xs, xs)

findRow :: Bool -> [([Bit], a)] -> a
findRow greatest = snd . head . until ((== 1) . length) (next greatest)

next :: Bool -> [([Bit], a)] -> [([Bit], a)]
next greatest bits =
  map (first tail) $ selectBy (comparing length) [zeros, ones]
  where
    selectBy = if greatest then maximumBy else minimumBy
    (zeros, ones) = partition ((== B0) . head . fst) bits
