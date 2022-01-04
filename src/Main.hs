import Kit (runWithArgs)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runWithArgs
