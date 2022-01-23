import Control.Monad (when)
import Kit (executeSolver)
import Puzzles (Day (..), Part (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit (Assertion, Counts (errors, failures), Test (TestLabel), Testable (test), runTestTT, (@=?), (~:))
import Text.Printf (printf)

testPuzzle :: Day -> Part -> [Integer] -> [Assertion]
testPuzzle d p = zipWith (testSolver d p) (Nothing : map Just [1 ..])

testSolver :: Day -> Part -> Maybe Int -> Integer -> Assertion
testSolver d p ex res = executeSolver d p ex >>= (Right res @=?)

main :: IO Counts
main = do
  args <- getArgs
  res <-
    runTestTT $
      test $
        filterAll
          args
          [ "Day 01"
              ~: [ "Part 1" ~: testPuzzle Day01 Part1 [1451, 7],
                   "Part 2" ~: testPuzzle Day01 Part2 [1395, 5]
                 ],
            "Day 02"
              ~: [ "Part 1" ~: testPuzzle Day02 Part1 [1451208, 150],
                   "Part 2" ~: testPuzzle Day02 Part2 [1620141160, 900]
                 ],
            "Day 03"
              ~: [ "Part 1" ~: testPuzzle Day03 Part1 [4191876, 198],
                   "Part 2" ~: testPuzzle Day03 Part2 [3414905, 230]
                 ],
            "Day 04"
              ~: [ "Part 1" ~: testPuzzle Day04 Part1 [8580, 4512],
                   "Part 2" ~: testPuzzle Day04 Part2 [9576, 1924]
                 ],
            "Day 18"
              ~: [ "Part 1" ~: testPuzzle Day18 Part1 [3816, 4140],
                   "Part 2" ~: testPuzzle Day18 Part2 [4819, 3993]
                 ],
            "Day 21"
              ~: [ "Part 1" ~: testPuzzle Day21 Part1 [893700, 739785],
                   "Part 2" ~: testPuzzle Day21 Part2 [568867175661958, 444356092776315]
                 ],
            "Day 22"
              ~: [ "Part 1" ~: testPuzzle Day22 Part1 [582644, 39, 590784, 474140],
                   "Part 2" ~: testPuzzle Day22 Part2 [1263804707062415, 39, 39769202357779, 2758514936282235]
                 ]
          ]
  when (failures res > 0 || errors res > 0) exitFailure
  return res

filterAll :: [String] -> [Test] -> [Test]
filterAll [] = id
filterAll [day] = filter $ filterTest $ printf "Day %02d" (read day :: Int)
filterAll [day, count] = take (read count) . cycle . filterAll [day]
filterAll _ = error "Too many arguments"

filterTest :: String -> Test -> Bool
filterTest n (TestLabel label _) = label == n
filterTest _ _ = True
