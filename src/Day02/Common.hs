module Day02.Common (Command (..), parse) where

data Command = Forward Int | Down Int | Up Int
  deriving (Show)

parse :: [String] -> [Command]
parse = map $ toCommand . words

toCommand :: [String] -> Command
toCommand ["forward", value] = Forward $ read value
toCommand ["down", value] = Down $ read value
toCommand ["up", value] = Up $ read value
toCommand _ = error "Invalid argument"
