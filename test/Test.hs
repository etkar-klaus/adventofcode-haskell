import Control.Monad (when)
import Kit (executeSolver)
import Puzzles (Day (..), Part (..))
import System.Exit (exitFailure)
import Test.HUnit (Assertion, Counts (errors, failures), Testable (test), runTestTT, (@=?), (~:))

testPuzzle :: Day -> Part -> [Integer] -> [Assertion]
testPuzzle d p = zipWith (testSolver d p) (Nothing : map Just [1 ..])

testSolver :: Day -> Part -> Maybe Int -> Integer -> Assertion
testSolver d p ex res = executeSolver d p ex >>= (Right res @=?)

main :: IO Counts
main = do
  res <-
    runTestTT $
      test
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
               ]
        ]
  when (failures res > 0 || errors res > 0) exitFailure
  return res
