module Day18.Common (magnitude, parse, plus) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (ord)

data Token = Open | Close | Number Int
  deriving (Show)

data CursorList = CursorList {_front, _back :: [Token]}
  deriving (Show)

forward :: CursorList -> CursorList
forward (CursorList front (x : back)) = CursorList (x : front) back
forward _ = error "already at end"

cursor :: [Token] -> CursorList
cursor = CursorList []

uncursor :: CursorList -> [Token]
uncursor (CursorList [] back) = back
uncursor (CursorList (x : front) back) = uncursor (CursorList front (x : back))

parse :: String -> [Token]
parse = map toToken . filter (/= ',')
  where
    toToken '[' = Open
    toToken ']' = Close
    toToken c = Number (ord c - ord '0')

plus :: [Token] -> [Token] -> [Token]
plus list1 list2 = reduce (Open : list1 ++ list2 ++ [Close])

reduce :: [Token] -> [Token]
reduce list = maybe list reduce (explodeMaybe list <|> splitMaybe list)

explodeMaybe :: [Token] -> Maybe [Token]
explodeMaybe = fmap (uncursor . explode) . (findExplode . cursor)

findExplode :: CursorList -> Maybe CursorList
findExplode = find (0 :: Int)
  where
    find _ (CursorList _ []) = Nothing
    find depth list@(CursorList _ (Open : _)) = find (depth + 1) (forward list)
    find depth list@(CursorList _ (Close : _))
      | depth > 4 = Just list
      | otherwise = find (depth - 1) (forward list)
    find depth list = find depth (forward list)

explode :: CursorList -> CursorList
explode (CursorList ((Number b) : (Number a) : _ : front) (_ : back)) = CursorList (inc a front) (Number 0 : inc b back)
explode _ = error "cannot explode"

splitMaybe :: [Token] -> Maybe [Token]
splitMaybe = fmap (uncursor . split) . (findSplit . cursor)

findSplit :: CursorList -> Maybe CursorList
findSplit (CursorList _ []) = Nothing
findSplit list@(CursorList _ (Number x : _))
  | x >= 10 = Just list
  | otherwise = findSplit (forward list)
findSplit list = findSplit (forward list)

split :: CursorList -> CursorList
split (CursorList front (Number x : back)) = CursorList front (Open : Number (x `div` 2) : Number ((x + 1) `div` 2) : Close : back)
split _ = error "cannot split"

inc :: Int -> [Token] -> [Token]
inc v = uncursor . inc' v . cursor
  where
    inc' _ list@(CursorList _ []) = list
    inc' value (CursorList front (Number x : back)) = CursorList front (Number (x + value) : back)
    inc' value list = inc' value (forward list)

magnitude :: [Token] -> Int
magnitude = magnitude' []
  where
    magnitude' stack (Number x : xs) = magnitude' (x : stack) xs
    magnitude' stack (Open : xs) = magnitude' stack xs
    magnitude' (a : b : stack) (Close : xs) = magnitude' (a * 2 + b * 3 : stack) xs
    magnitude' [a] [] = a
    magnitude' _ _ = error "invalid tokens"
