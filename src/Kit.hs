module Kit (executeSolver, runWithArgs) where

import Control.Exception (try)
import Puzzles (Day (..), Part (..), solversForDay)
import Text.Printf (printf)
import Text.Read (readEither)

runWithArgs :: [String] -> IO ()
runWithArgs args =
  case parseArgs args of
    Right (d, p, ex) -> either Left Right <$> executeSolver d p ex
    Left err -> return $ Left err
    >>= out

executeSolver :: Day -> Part -> Maybe Int -> IO (Either String Integer)
executeSolver d p ex =
  either (Left . show) (Right . solver d p . lines)
    <$> (try $ readFile ("data/" ++ file d ex ++ ".txt") :: IO (Either IOError String))

solver :: Day -> Part -> [String] -> Integer
solver d Part1 = fst $ solversForDay d
solver d Part2 = snd $ solversForDay d

parseArgs :: [String] -> Either String (Day, Part, Maybe Int)
parseArgs args = do
  (dArg, dTail) <- headEither args "Please specify day and part"
  d <- parseEnum dArg (maxBound :: Day) "Invalid day"
  (pArg, pTail) <- headEither dTail "Please specify part"
  p <- parseEnum pArg (maxBound :: Part) "Invalid part"
  let (exArg, exTail) = headMaybe pTail
  ex <- maybe (Right Nothing) parseExample exArg
  ifEither null "Too many arguments" (const (d, p, ex)) exTail

headEither :: [a] -> String -> Either String (a, [a])
headEither [] err = Left err
headEither (x : xs) _ = Right (x, xs)

headMaybe :: [a] -> (Maybe a, [a])
headMaybe [] = (Nothing, [])
headMaybe (x : xs) = (Just x, xs)

parseEnum :: Enum a => String -> a -> String -> Either String a
parseEnum d bound err = prefixError err $ either Left (toEnumEither bound) (readEither d)

toEnumEither :: Enum a => a -> Int -> Either String a
toEnumEither bound idx
  | idx < 1 = Left "must be at least 1"
  | idx > m = Left ("must be at most " ++ show m)
  | otherwise = Right $ toEnum (idx - 1)
  where
    m = fromEnum bound + 1

parseExample :: String -> Either String (Maybe Int)
parseExample =
  prefixError "Invalid example"
    . either Left (ifEither (> 0) "must be at least 1 (or nothing)" Just)
    . readEither

ifEither :: (a -> Bool) -> String -> (a -> b) -> a -> Either String b
ifEither test left rightF value = if test value then Right $ rightF value else Left left

prefixError :: String -> Either String a -> Either String a
prefixError err = either (Left . (err ++) . (": " ++)) Right

file :: Day -> Maybe Int -> String
file d ex = printf "day%02d" (fromEnum d + 1) ++ maybe "" (printf "example%d") ex

out :: Either String Integer -> IO ()
out = either print print . prefixError "Error"
