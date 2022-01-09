{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day22.Common (Cube (..), combine, intersects, parseAll, totalVolume) where

import Control.Applicative (Applicative (liftA2))
import Data.Foldable (Foldable (foldl'))
import Text.Regex.Posix ((=~))

data Cube = Cube {-on/off ::-} Bool {-x1 ::-} Int {-x2 ::-} Int {-y1 ::-} Int {-y2 ::-} Int {-z1 ::-} Int {-z2 ::-} Int
  deriving (Show)

parseAll :: [String] -> [Cube]
parseAll = map parse

parse :: String -> Cube
parse line =
  let (_, _, _, groups) = line =~ "(on|off) x=(-?[0-9]+)\\.\\.(-?[0-9]+),y=(-?[0-9]+)\\.\\.(-?[0-9]+),z=(-?[0-9]+)\\.\\.(-?[0-9]+)" :: (String, String, String, [String])
   in parseCube groups

parseCube :: [String] -> Cube
parseCube (on : xs) = parseOrdinates (Cube (on == "on")) $ map read xs
parseCube [] = error "invalid arguments"

parseOrdinates :: (t -> t -> t -> t -> t -> t -> p) -> [t] -> p
parseOrdinates c [x1, x2, y1, y2, z1, z2] = c x1 x2 y1 y2 z1 z2
parseOrdinates _ _ = error "invalid arguments"

combine :: [Cube] -> [Cube]
combine =
  let add cube = liftA2 (++) (addWhenOn cube . intersectAll cube) id
   in foldl' (flip add) []

addWhenOn :: Cube -> [Cube] -> [Cube]
addWhenOn cube@(Cube True _ _ _ _ _ _) = (:) cube
addWhenOn _ = id

intersectAll :: Cube -> [Cube] -> [Cube]
intersectAll cube =
  map (intersection cube) . filter (intersects cube)

intersects :: Cube -> Cube -> Bool
intersects (Cube _ x11 x12 y11 y12 z11 z12) (Cube _ x21 x22 y21 y22 z21 z22) =
  x11 <= x22 && x21 <= x12 && y11 <= y22 && y21 <= y12 && z11 <= z22 && z21 <= z12

intersection :: Cube -> Cube -> Cube
intersection (Cube _ x11 x12 y11 y12 z11 z12) (Cube on x21 x22 y21 y22 z21 z22) =
  Cube
    (not on)
    (max x11 x21)
    (min x12 x22)
    (max y11 y21)
    (min y12 y22)
    (max z11 z21)
    (min z12 z22)

totalVolume :: [Cube] -> Integer
totalVolume = sum . map volume

volume :: Cube -> Integer
volume (Cube on x1 x2 y1 y2 z1 z2) =
  toInteger (x2 - x1 + 1) * toInteger (y2 - y1 + 1) * toInteger (z2 - z1 + 1) * (if on then 1 else -1)
