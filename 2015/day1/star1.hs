import System.Environment
import System.IO
import Data.List

main = do
    args <- getArgs
    contents <- readFile (head args)
    let count = countFloors contents
    print count

countFloors :: String -> Int
countFloors "\n" = 0
countFloors ['('] = 1
countFloors [')'] = -1
countFloors (x:xs) = countFloors [x] + countFloors xs
